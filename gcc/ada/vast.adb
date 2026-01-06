------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 V A S T                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2020-2026, Free Software Foundation, Inc.      --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

pragma Unsuppress (All_Checks);
pragma Assertion_Policy (Check);
--  Enable checking. This isn't really necessary, but it might come in handy if
--  we want to run VAST with a compiler built without checks. Anyway, it's
--  harmless, because VAST is not run by default.

with Ada.Unchecked_Deallocation;

with System.Case_Util;

with Atree;          use Atree;
with Debug;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils; use Einfo.Utils;
with Errout;
with Exp_Ch6;
with Exp_Tss;
with Lib;            use Lib;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Opt;            use Opt;
with Output;
with Sem_Aux;
with Sem_Util;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinput;
with Table;
with Types;          use Types;

package body VAST is

   --  ???Basic tree properties not yet checked:
   --   - No dangling trees. Every node that is reachable at all is reachable
   --     by some syntactic path.
   --   - Basic properties of Nlists/Elists (next/prev pointers make sense,
   --     for example).

   Force_Enable_VAST : constant Boolean := False;
   --  Normally, VAST is enabled by the the -gnatd_V switch.
   --  To force it to be enabled independent of any switches,
   --  set this to True.

   type Check_Enum is
     (Check_Other,
      Check_Sloc,
      Check_Analyzed,
      Check_Error_Nodes,
      Check_FE_Only,
      Check_Sharing,
      Check_Parent_Present,
      Check_Parent_Correct,
      Check_Scope_Present,
      Check_Scope_Correct);

   type Check_Status is
     --  Action in case of check failure:
     (Disabled, -- Do nothing
      Enabled, -- Print messages, and raise an exception
      Print_And_Continue); -- Print a message

   pragma Warnings (Off, "Status*could be declared constant");
   --  Status is variable so we can modify it in gdb, for example
   Status : array (Check_Enum) of Check_Status :=
     (Check_Other => Enabled,
      Check_Sloc => Disabled,
      Check_Analyzed => Disabled,
      Check_Error_Nodes => Enabled,
      Check_FE_Only => Disabled,
      Check_Sharing => Disabled,
      Check_Parent_Present => Enabled,
      Check_Parent_Correct => Disabled,
      Check_Scope_Present => Print_And_Continue,
      Check_Scope_Correct => Print_And_Continue);
--      others => Print_And_Continue);
--      others => Enabled);
--      others => Disabled);
   --  Passing checks are Check_Other, which should always be Enabled.
   --  Currently-failing checks are different enumerals in Check_Enum,
   --  which can be disabled individually until we fix the bugs, or enabled
   --  when debugging particular bugs. Pass a nondefault Check_Enum to
   --  Assert in order to deal with bugs we have not yet fixed,
   --  and play around with the value of Status above for
   --  testing and debugging.
   --
   --  Note: Once a bug is fixed, and the check passes reliably, we may choose
   --  to remove that check from Check_Enum and use Check_Other instead.

   type Node_Stack_Index is new Pos;
   subtype Node_Stack_Count is
     Node_Stack_Index'Base range 0 .. Node_Stack_Index'Last;

   package Node_Stack is new Table.Table
     (Table_Component_Type => Node_Id,
      Table_Index_Type     => Node_Stack_Index'Base,
      Table_Low_Bound      => 1,
      Table_Initial        => 1,
      Table_Increment      => 100,
      Table_Name           => "Node_Stack");

   type Pass_Number is range 1 .. 2;
   Pass : Pass_Number;

   procedure VAST;
   --  Called by VAST_If_Enabled to do all the checking

   procedure Fail
     (Check : Check_Enum := Check_Other;
      Detail : String := "");
   --  Print failure information if Check is not disabled. Called by Assert
   --  when Condition is False and for other failures.

   procedure Fail_Breakpoint (N : Node_Id) with Export;
   --  Does nothing. Called by Fail; useful to set a breakpoint in gdb on this.

   procedure Assert
     (Condition : Boolean;
      Check : Check_Enum := Check_Other;
      Detail : String := "");
   --  Check that the Condition is True. Status determines action on failure.
   --  Note: This procedure is used to detect errors in the tree, whereas
   --  pragma Assert is used to detect errors in VAST itself.

   function To_Mixed (A : String) return String;
   --  Copied from System.Case_Util; old versions of that package do not have
   --  this function, so this is needed for bootstrapping.

   function Image (Kind : Node_Kind) return String is (To_Mixed (Kind'Img));
   function Image (Kind : Entity_Kind) return String is (To_Mixed (Kind'Img));
   function Kind_Image (N : Node_Or_Entity_Id) return String is
     (if Nkind (N) in N_Entity then Image (Ekind (N))
      else Image (Nkind (N)));
   function Node_Image (N : Node_Or_Entity_Id) return String is
     (Kind_Image (N) & N'Img);

   procedure Put (S : String);
   procedure Put_Line (S : String);
   procedure Put_Node (N : Node_Id);
   procedure Put_Node_Stack;
   --  Output routines; print only if -gnatd_W (VAST in verbose mode) is
   --  enabled.

   procedure Put_Indentation;
   --  Print spaces to indicate nesting depth of Node_Stack

   procedure Enter_Node (N : Node_Id);
   procedure Leave_Node (N : Node_Id);
   --  Called for each node while walking the tree.
   --  Push/pop N to/from Node_Stack.
   --  Print enter/leave debugging messages.
   --  ???Possible improvements to messages:
   --    Walk subtrees in a better order.
   --    Print field names.
   --    Don't print boring fields (such as N_Empty nodes).
   --    Print more info (value of literals, "A.B.C" for expanded names, etc.).
   --    Share some code with Treepr.

   procedure Do_Tree (N : Node_Id);
   --  Do VAST checking on a tree of nodes

   function Is_FE_Only (Kind : Node_Kind) return Boolean;
   --  True if nodes of this Kind can appear only in the front end. They should
   --  be transformed into something else before calling the back end, or else
   --  they can only appear in illegal code.

   function Has_Subtrees (N : Node_Id) return Boolean;
   --  True if N has one or more syntactic fields

   procedure Do_Subtrees (N : Node_Id);
   --  Call Do_Tree on all the subtrees (i.e. syntactic fields) of N

   procedure Do_List (L : List_Id);
   --  Call Do_Tree on the list elements

   procedure Do_Node_Pass_2 (N : Node_Id);
   --  Called by Do_Tree in the second pass

   procedure Do_Unit (U : Unit_Number_Type);
   --  Call Do_Tree on the root node of a compilation unit

   function Is_On_Stack (Kind : Node_Kind) return Boolean;
   --  True if there is at least one node on the stack with the specified Kind

   function Ancestor_Node (Count : Node_Stack_Count) return Node_Id;
   --  Nth ancestor on the Node_Stack. Ancestor_Node(0) is the current node,
   --  Ancestor_Node(1) is its parent, Ancestor_Node(2) is its grandparent,
   --  and so on.

   function Top_Node return Node_Id is (Ancestor_Node (0));

   type Node_Info is record
      Count : Nat := 0;
      Prev_Parent : Node_Id := Empty;
      In_Aspect : Boolean := False;
   end record;
   type Node_Info_Array is array (Node_Id range <>) of Node_Info;
   type Node_Info_Array_Ptr is access all Node_Info_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Node_Info_Array, Node_Info_Array_Ptr);

   Nodes_Info : Node_Info_Array_Ptr;
   --  Nodes_Info (N).Prev_Parent is non-Empty if and only if the tree walk has
   --  visited N. If non-Empty, it points to the most recent parent of N in the
   --  tree walk; that is, the node that allowed us to get to N. Normally, each
   --  reachable node is visited exactly once, and if the Parent pointers
   --  aren't messed up, then Nodes_Info (N).Prev_Parent will be Parent (N).
   --  (See below for the special case of the root compilation unit node.)
   --
   --  Used to detect incorrect sharing of subtrees or (worse) cycles. We don't
   --  allocate this on the stack, for fear of Storage_Error.
   --
   --  Nodes_Info (N).Count is the number of ways N is reachable in the walk.
   --  It should be 1 for all nodes except the root.

   function Get_Node_Field_Union is new
     Atree.Atree_Private_Part.Get_32_Bit_Field (Union_Id) with Inline;

   function Has_Field (Kind : Node_Kind; F : Node_Field) return Boolean;
   --  True if nodes of type Kind have field F

   function Related_Chars (N : Node_Id) return Name_Id;
   --  Return a Name_Id related to N that is worth printing when we print
   --  information about N. Returns No_Name if there is no interesting Name_Id.
   --  This is typically "Chars (N)" or "Chars (Defining_Identifier (N))" or
   --  similar.

   procedure Check_Scope (N : Node_Id);
   --  Check that the Scope of N makes sense

   procedure Validate_Subprogram_Calls (N : Node_Id);
   --  Check that the number of actuals (including extra actuals) of all calls
   --  within N match their corresponding formals; check also that the names
   --  of BIP extra actuals and formals match.

   --------------
   -- To_Mixed --
   --------------

   function To_Mixed (A : String) return String is
      Result : String := A;
   begin
      System.Case_Util.To_Mixed (Result);
      return Result;
   end To_Mixed;

   ---------
   -- Put --
   ---------

   procedure Put (S : String) is
   begin
      if Debug.Debug_Flag_Underscore_WW then
         Output.Write_Str (S);
      end if;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (S : String) is
   begin
      if Debug.Debug_Flag_Underscore_WW then
         Output.Write_Line (S);
      end if;
   end Put_Line;

   ---------------
   -- Has_Field --
   ---------------

   function Has_Field (Kind : Node_Kind; F : Node_Field) return Boolean is
      Fields : Node_Field_Array renames Node_Field_Table (Kind).all;
   begin
      for Index in Fields'Range loop
         if Fields (Index) = F then
            return True;
         end if;
      end loop;

      return False;
   end Has_Field;

   -------------------
   -- Related_Chars --
   -------------------

   function Related_Chars (N : Node_Id) return Name_Id is
   begin
      return Result : Name_Id := No_Name do
         if Has_Field (Nkind (N), F_Chars) then
            Result := Chars (N);
         elsif Has_Field (Nkind (N), F_Defining_Identifier) then
            Result := Related_Chars (Defining_Identifier (N));
         elsif Has_Field (Nkind (N), F_Defining_Unit_Name) then
            Result := Related_Chars (Defining_Unit_Name (N));
         elsif Has_Field (Nkind (N), F_Specification) then
            Result := Related_Chars (Specification (N));
         end if;
      end return;
   end Related_Chars;

   --------------
   -- Put_Node --
   --------------

   procedure Put_Node (N : Node_Id) is
   begin
      if Debug.Debug_Flag_Underscore_WW then
         Put (Node_Image (N) & " ");
         Sinput.Write_Location (Sloc (N));

         if Comes_From_Source (N) then
            Put (" (s)");
         end if;

         declare
            Chars_To_Print : constant Name_Id := Related_Chars (N);
         begin
            if Present (Chars_To_Print) then
               Put (" ");
               Write_Name_For_Debug (Chars_To_Print, Quote => """");
            end if;
         end;
      end if;
   end Put_Node;

   ---------------------
   -- Put_Indentation --
   ---------------------

   procedure Put_Indentation is
   begin
      Put (String'(Natural (Node_Stack.First) ..
                   Natural (Node_Stack.Last) * 2 => ' '));
   end Put_Indentation;

   ----------------
   -- Enter_Node --
   ----------------

   procedure Enter_Node (N : Node_Id) is
   begin
      Node_Stack.Append (N); -- push

      if Has_Subtrees (N) then
         Put ("-->");
      else
         --  If no subtrees, just print one line for enter/leave
         Put ("   ");
      end if;
      Put_Indentation;
      Put_Node (N);
      Put_Line ("");
   end Enter_Node;

   ----------------
   -- Leave_Node --
   ----------------

   procedure Leave_Node (N : Node_Id) is
   begin
      if Has_Subtrees (N) then
         Put ("<--");
         Put_Indentation;
         Put_Node (N);
         Put_Line ("");
      end if;

      Node_Stack.Decrement_Last; -- pop
   end Leave_Node;

   --------------------
   -- Put_Node_Stack --
   --------------------

   procedure Put_Node_Stack is
   begin
      for J in reverse Node_Stack.First .. Node_Stack.Last loop
         Put_Node (Node_Stack.Table (J));
         Put_Line ("");
      end loop;
   end Put_Node_Stack;

   -----------------
   -- Is_On_Stack --
   -----------------

   function Is_On_Stack (Kind : Node_Kind) return Boolean is
   begin
      for J in reverse Node_Stack.First .. Node_Stack.Last loop
         if Nkind (Node_Stack.Table (J)) = Kind then
            return True;
         end if;
      end loop;

      return False;
   end Is_On_Stack;

   -------------------
   -- Ancestor_Node --
   -------------------

   function Ancestor_Node (Count : Node_Stack_Count) return Node_Id is
   begin
      return Node_Stack.Table (Node_Stack.Last - Count);
   end Ancestor_Node;

   ---------------------
   -- Fail_Breakpoint --
   ---------------------

   procedure Fail_Breakpoint (N : Node_Id) is
   begin
      null;
   end Fail_Breakpoint;

   ----------
   -- Fail --
   ----------

   VAST_Failure : exception;

   procedure Fail
     (Check : Check_Enum := Check_Other;
      Detail : String := "")
   is
      Part1 : constant String := "VAST fail";
      Part2 : constant String :=
        (if Check = Check_Other then ""
         else ": " & To_Mixed (Check'Img));
      Part3 : constant String :=
        (if Detail = "" then "" else " -- " & Detail);
      Message : constant String := Part1 & Part2 & Part3;
      Save : constant Boolean := Debug.Debug_Flag_Underscore_WW;
   begin
      case Status (Check) is
         when Disabled => null;
         when Enabled | Print_And_Continue =>
            Debug.Debug_Flag_Underscore_WW := True;
            --  ????We should probably avoid changing the debug flag here
            Put (Message & ": ");
            Put_Node (Top_Node);
            Put_Line ("");

            Put ("VAST file: ");
            Sinput.Write_Location (Sloc (Top_Node));
            Put_Line ("");
            Put_Node_Stack;

            if Status (Check) = Enabled then
               Put_Node_Stack;
               raise VAST_Failure with Message;
            end if;

            Debug.Debug_Flag_Underscore_WW := Save;

            Fail_Breakpoint (Ancestor_Node (0));
      end case;
   end Fail;

   ------------
   -- Assert --
   ------------

   procedure Assert
     (Condition : Boolean;
      Check : Check_Enum := Check_Other;
      Detail : String := "")
   is
   begin
      if not Condition then
         Fail (Check, Detail);
      end if;
   end Assert;

   -----------------
   -- Check_Scope --
   -----------------

   procedure Check_Scope (N : Node_Id) is
      use Exp_Tss, Sem_Util;
   begin
      if Present (Scope (N)) then
         if False then -- ????
            Assert (Enclosing_Declaration (Scope (N)) =
                    Enclosing_Declaration (Enclosing_Declaration (N)),
                    Check_Scope_Correct);
         end if;
      else
         if Ekind (N) = E_Void then
            --  ????These seem to be SW, PI, &c, and their params.
            null;
         elsif Ekind (N) = E_Procedure and then Is_TSS (N, TSS_Put_Image)
         then
            null; -- also PI
         elsif Ekind (N) = E_Protected_Body then
            null;
         else
            Fail (Check_Scope_Present);
         end if;
      end if;
   end Check_Scope;

   --------------------
   -- Do_Node_Pass_2 --
   --------------------

   procedure Do_Node_Pass_2 (N : Node_Id) is
   begin
      --  Check Sloc

      case Nkind (N) is
         --  ???Some nodes, including exception handlers, have no Sloc;
         --  it's unclear why.

         when N_Exception_Handler =>
            Assert
              ((if Comes_From_Source (N) then Present (Sloc (N))), Check_Sloc);
         when others =>
            Assert (Present (Sloc (N)), Check_Sloc);
      end case;

      --  All reachable nodes should have been analyzed by the time we get
      --  here.

      Assert (Analyzed (N), Check_Analyzed);

      --  Misc checks based on node/entity kind

      case Nkind (N) is
         when N_Unused_At_Start | N_Unused_At_End =>
            --  ????Can't get here, because Is_FE_Only. Also 'case' below.
            Fail;

         when N_Error =>
            --  VAST doesn't do anything when Serious_Errors_Detected > 0 (at
            --  least for now), so we shouldn't encounter any N_Error nodes.
            Fail (Check_Error_Nodes);

         when N_Entity =>
            Check_Scope (N);

            case Ekind (N) is
               when others =>
                  null; -- more to be done here
            end case;

         when others =>
            null; -- more to be done here
      end case;

      --  Check that N has a Parent, except in certain cases

      case Nkind (N) is
         when N_Empty =>
            raise Program_Error; -- can't get here

         when N_Error =>
            Fail (Check_Error_Nodes);
            --  The error node has no parent, but we shouldn't even be seeing
            --  error nodes in VAST at all. See earlier "when N_Error".

         when N_Compilation_Unit =>
            Assert (No (Parent (N)));
            --  The parent of the root of each unit is empty.

         when N_Entity =>
            if not Is_Itype (N) then
               --  An Itype might or might not have a parent

               Assert
                 (Present (Parent (N)), Detail => "missing parent of entity");
               Assert (Parent (N) = Ancestor_Node (1), Check_Parent_Correct);
            end if;

         when others =>
            Assert (Present (Parent (N)), Check_Parent_Present);
            --  All other nodes should have a parent
            if Status (Check_Parent_Present) = Enabled then
               Assert (Parent (N) = Ancestor_Node (1), Check_Parent_Correct);
            end if;
      end case;
   end Do_Node_Pass_2;

   -------------
   -- Do_Tree --
   -------------

   procedure Do_Tree (N : Node_Id) is
      Visited : constant Boolean := Present (Nodes_Info (N).Prev_Parent);
   begin
      if False and Nkind (N) = N_Aspect_Specification then
         --  ????This cuts failures 453490/235214 = 1.9.
         return;
      end if;

      if Pass = 1 then
         Nodes_Info (N).Count := Nodes_Info (N).Count + 1;
         --  ????Get rid of asserts:
         pragma Assert
           (if Nkind (N) not in N_Empty | N_Compilation_Unit then
              Visited = (Nodes_Info (N).Count > 1));

         if Is_On_Stack (N_Aspect_Specification) then
            Nodes_Info (N).In_Aspect := True;
         end if;
      elsif Pass = 2 then
         pragma Assert (Nodes_Info (N).Count > 0);
      end if;

      Enter_Node (N);

      Assert (not Is_FE_Only (Nkind (N)), Check_FE_Only);
      --  ????Also check for particular pragmas, etc.
      --  And Ekind.

      if Nkind (N) = N_Empty then
         Assert (N = Empty);
         Assert (No (Sloc (N)));
         goto Done; -- -------------->
         --  Don't do any further checks on Empty
      end if;

      --  If we visit the same node more than once, then there are shared
      --  nodes; the "tree" is not a tree:
      --  We know that the "extra formals" involve shared subtrees,
      --  and that's probably unavoidable. See Expand_Call_Helper.
      --  A lot of shared subtrees come from aspect specifications,
      --  probably because they get turned into pragmas, and the
      --  subtrees get placed inside the pragmas without removing
      --  them from the original aspect specifications.

      if Pass = 2 and then Nodes_Info (N).Count > 1 and then
        not Nodes_Info (N).In_Aspect -- ????cuts failures by 1.9
      then
         declare
            Count : constant String :=
              (if Nodes_Info (N).Count = 2 then ""
               else Nodes_Info (N).Count'Img & "par");
            Aspect : constant String :=
              (if Nodes_Info (N).In_Aspect then "{asp}" else "");
         begin
            Fail (Check_Sharing,
                  "(prev-par=" &
                  Node_Image (Nodes_Info (N).Prev_Parent) & ")" &
                  Count & Aspect);
            if Status (Check_Sharing) /= Disabled then
               Output.Write_Line
                 (Kind_Image (Ancestor_Node (1)) & "```" & Kind_Image (N));
               Output.Write_Line ("");
            end if;
         end;
      end if;

      if Node_Stack.Last = 1 then
         Nodes_Info (N).Prev_Parent := Ancestor_Node (0);
         Assert (Nkind (N) = N_Compilation_Unit);
         --  This is the root node. Set the parent to itself,
         --  for no particular reason except to make it not Empty.
      else
         Nodes_Info (N).Prev_Parent := Ancestor_Node (1);
      end if;

      if not Visited then -- Don't walk it more than once
         if Pass = 2 then
            Do_Node_Pass_2 (N);
         end if;
         Do_Subtrees (N);
      end if;
      <<Done>>

      Leave_Node (N);
   end Do_Tree;

   -----------------
   -- Has_Subtrees --
   -----------------

   function Has_Subtrees (N : Node_Id) return Boolean is
      Offsets : Traversed_Offset_Array renames
        Traversed_Fields (Nkind (N));
   begin
      --  True if the first Offset is not the sentinel
      return Offsets (Offsets'First) /= No_Field_Offset;
   end Has_Subtrees;

   -----------------
   -- Do_Subtrees --
   -----------------

   procedure Do_Subtrees (N : Node_Id) is
      --  ???Do we need tail recursion elimination here,
      --  as in Atree.Traverse_Func?
      Offsets : Traversed_Offset_Array renames
        Traversed_Fields (Nkind (N));
   begin
      for Cur_Field in Offset_Array_Index loop
         exit when Offsets (Cur_Field) = No_Field_Offset;

         declare
            F : constant Union_Id :=
              Get_Node_Field_Union (N, Offsets (Cur_Field));
         begin
            if F in Node_Range then
               Do_Tree (Node_Id (F));
            elsif F in List_Range then
               Do_List (List_Id (F));
            else
               raise Program_Error;
            end if;
         end;
      end loop;
   end Do_Subtrees;

   -------------
   -- Do_List --
   -------------

   procedure Do_List (L : List_Id) is
      Elmt : Node_Id := First (L);
      Len : constant String := List_Length (L)'Img;
   begin
      if Is_Non_Empty_List (L) then
         Put ("-->");
         Put_Indentation;
         Put_Line ("list len=" & Len);

         while Present (Elmt) loop
            Do_Tree (Elmt);
            Next (Elmt);
         end loop;

         Put ("<--");
         Put_Indentation;
         Put_Line ("list len=" & Len);
      end if;
   end Do_List;

   -------------
   -- Do_Unit --
   -------------

   procedure Do_Unit (U : Unit_Number_Type) is
      U_Name : constant Unit_Name_Type := Unit_Name (U);
      U_Name_S : constant String :=
        (if U_Name = No_Unit_Name then "<No_Unit_Name>"
         else Get_Name_String (U_Name));
      Predef : constant String :=
        (if Is_Predefined_Unit (U) then " (predef)"
         elsif Is_Internal_Unit (U) then " (gnat)"
         else "");
      Is_Main : constant String :=
        (if U = Main_Unit then " (main unit)" else "");
      Msg : constant String :=
        "VAST for unit" & U'Img & " " & U_Name_S & Predef & Is_Main;

      Is_Preprocessing_Dependency : constant Boolean := U_Name = No_Unit_Name;
      --  True if this is a bogus unit added by Add_Preprocessing_Dependency.
      --  These units have no name and no associated tree; we had better not
      --  try to walk nonexistent trees.

      Root : constant Node_Id := Cunit (U);
   begin
      pragma Assert (Node_Stack.Last = 0);
      Assert (No (Root) = Is_Preprocessing_Dependency);
      --  All compilation units except these bogus ones should have a Cunit.

      Put_Line (Msg);

      if Is_Preprocessing_Dependency then
         Put_Line ("Skipping preprocessing dependency");
         return;
      end if;

      Assert (Present (Root));
      Do_Tree (Root);
      Put_Line (Msg & "  (done)");
      pragma Assert (Node_Stack.Last = 0);
   end Do_Unit;

   ----------
   -- VAST --
   ----------

   procedure VAST is
   begin
      Put_Line ("VAST");

      --  Operating_Mode = Generate_Code implies there are no legality errors

      pragma Assert (Serious_Errors_Detected = 0);
      pragma Assert (not Errout.Compilation_Errors);

      Put_Line ("VAST checking" & Last_Unit'Img & " units");

      declare
         use Atree_Private_Part;
         Last_Node : constant Node_Id := Node_Offsets.Last;
      begin
         pragma Assert (Nodes_Info = null);
         Nodes_Info := new Node_Info_Array (Node_Id'First .. Last_Node);

         --  Walk all nodes in all units doing Pass 1, and so on
         --  for each Pass.

         for P in Pass_Number loop
            Pass := P;

            Put_Line ("VAST Pass" & Pass'Img);
            if Pass = 2 then -- ????Is this needed?
               for Index in Nodes_Info'Range loop
                  Nodes_Info (Index).Prev_Parent := Empty;
               end loop;
            end if;

            for U in Main_Unit .. Last_Unit loop
               --  Main_Unit is the one passed to the back end, but here we are
               --  walking all the units.
               Do_Unit (U);
            end loop;
         end loop;

         --  Validate subprogram calls; check "extra formals". This works only
         --  for the main unit.

         Validate_Subprogram_Calls (Cunit (Main_Unit));

         --  We shouldn't have allocated any new nodes during VAST

         pragma Assert (Node_Offsets.Last = Last_Node);
         Free (Nodes_Info);
      end;

      Put_Line ("VAST done.");
   end VAST;

   ---------------------
   -- VAST_If_Enabled --
   ---------------------

   procedure VAST_If_Enabled is
      --  This is the public entry point

      pragma Assert (Expander_Active = (Operating_Mode = Generate_Code));
      --  ???So why do we need both Operating_Mode and Expander_Active?
      use Debug;
   begin
      --  Do nothing if we're not calling the back end; the main point of VAST
      --  is to protect against code-generation bugs. VAST is disabled if
      --  legality errors were detected; the tree is known to be malformed
      --  in some error cases. The -gnatc switch also disables VAST.

      if Operating_Mode /= Generate_Code then
         return;
      end if;

      --  If -gnatd_W (VAST in verbose mode) is enabled, then that should imply
      --  -gnatd_V (enable VAST).

      if Debug_Flag_Underscore_WW or Force_Enable_VAST then
         Debug_Flag_Underscore_VV := True;
      end if;

      --  Do nothing if VAST is disabled

      if not Debug_Flag_Underscore_VV then
         return;
      end if;

      VAST;
   end VAST_If_Enabled;

   -------------------------------
   -- Validate_Subprogram_Calls --
   -------------------------------

   procedure Validate_Subprogram_Calls (N : Node_Id) is
      use Sem_Aux, Sem_Util;

      function Process_Node (Nod : Node_Id) return Traverse_Result;
      --  Function to traverse the subtree of N using Traverse_Proc.

      ------------------
      -- Process_Node --
      ------------------

      function Process_Node (Nod : Node_Id) return Traverse_Result is
      begin
         case Nkind (Nod) is
            when N_Entry_Call_Statement
               | N_Procedure_Call_Statement
               | N_Function_Call
            =>
               declare
                  Call_Node : Node_Id renames Nod;
                  Subp      : constant Entity_Id := Get_Called_Entity (Nod);

               begin
                  pragma Assert (Exp_Ch6.Check_BIP_Actuals (Call_Node, Subp));

                  --  Build-in-place function calls return their result by
                  --  reference.

                  pragma Assert (not Exp_Ch6.Is_Build_In_Place_Function (Subp)
                    or else Returns_By_Ref (Subp));
               end;

            --  Skip generic bodies

            when N_Package_Body =>
               if Ekind (Unique_Defining_Entity (Nod)) = E_Generic_Package then
                  return Skip;
               end if;

            when N_Subprogram_Body =>
               if Ekind (Unique_Defining_Entity (Nod)) in E_Generic_Function
                                                        | E_Generic_Procedure
               then
                  return Skip;
               end if;

            --  Nodes we want to ignore

            --  Skip calls placed in the full declaration of record types since
            --  the call will be performed by their Init Proc; for example,
            --  calls initializing default values of discriminants or calls
            --  providing the initial value of record type components. Other
            --  full type declarations are processed because they may have
            --  calls that must be checked. For example:

            --    type T is array (1 .. Some_Function_Call (...)) of Some_Type;

            --  ??? More work needed here to handle the following case:

            --    type Rec is record
            --       F : String (1 .. <some complicated expression>);
            --    end record;

            when N_Full_Type_Declaration =>
               if Is_Record_Type (Defining_Entity (Nod)) then
                  return Skip;
               end if;

            --  Skip calls placed in unexpanded initialization expressions

            when N_Object_Declaration =>
               if No_Initialization (Nod) then
                  return Skip;
               end if;

            --  Skip calls placed in subprogram specifications since function
            --  calls initializing default parameter values will be processed
            --  when the call to the subprogram is found (if the default actual
            --  parameter is required), and calls found in aspects will be
            --  processed when their corresponding pragma is found, or in the
            --  specific case of class-wide pre-/postconditions, when their
            --  helpers are found.

            when N_Procedure_Specification
               | N_Function_Specification
            =>
               return Skip;

            when N_Abstract_Subprogram_Declaration
               | N_Aspect_Specification
               | N_At_Clause
               | N_Call_Marker
               | N_Empty
               | N_Enumeration_Representation_Clause
               | N_Enumeration_Type_Definition
               | N_Function_Instantiation
               | N_Freeze_Generic_Entity
               | N_Generic_Function_Renaming_Declaration
               | N_Generic_Package_Renaming_Declaration
               | N_Generic_Procedure_Renaming_Declaration
               | N_Generic_Package_Declaration
               | N_Generic_Subprogram_Declaration
               | N_Itype_Reference
               | N_Number_Declaration
               | N_Package_Instantiation
               | N_Package_Renaming_Declaration
               | N_Pragma
               | N_Procedure_Instantiation
               | N_Protected_Type_Declaration
               | N_Record_Representation_Clause
               | N_Validate_Unchecked_Conversion
               | N_Variable_Reference_Marker
               | N_Use_Package_Clause
               | N_Use_Type_Clause
               | N_With_Clause
            =>
               return Skip;

            when others =>
               null;
         end case;

         return OK;
      end Process_Node;

      procedure Check_Calls is new Traverse_Proc (Process_Node);

   --  Start of processing for Validate_Subprogram_Calls

   begin
      --  No action if we are not generating code (including if we have
      --  errors).

      if Operating_Mode /= Generate_Code then
         return;
      end if;

      pragma Assert (Serious_Errors_Detected = 0);

      --  Do not attempt to verify the return type in CodePeer_Mode
      --  as CodePeer_Mode is missing some expansion code that
      --  results in trees that would be considered malformed for
      --  GCC but aren't for GNAT2SCIL.

      if not CodePeer_Mode then
         Check_Calls (N);
      end if;
   end Validate_Subprogram_Calls;

   ----------------
   -- Is_FE_Only --
   ----------------

   function Is_FE_Only (Kind : Node_Kind) return Boolean is
      --  ????This is work in progress; see "?" marks below
   begin
      case Kind is
         when N_Abortable_Part
            | N_Abort_Statement
            | N_Asynchronous_Select
            | N_Compound_Statement
            | N_Conditional_Entry_Call
            | N_Continue_Statement
            | N_Contract
            | N_Delay_Alternative
            | N_Delay_Until_Statement
            | N_Delta_Constraint
            | N_Entry_Call_Alternative
            | N_Entry_Index_Specification
            | N_Error
            | N_Formal_Derived_Type_Definition
            | N_Formal_Package_Declaration
            | N_Goto_When_Statement
            | N_Interpolated_String_Literal
            | N_Iterated_Element_Association
            | N_Mod_Clause
            | N_Raise_When_Statement
            | N_Return_When_Statement
            | N_SCIL_Dispatching_Call
            | N_SCIL_Dispatch_Table_Tag_Init
            | N_SCIL_Membership_Test
            | N_Timed_Entry_Call
            | N_Triggering_Alternative
            | N_Unused_At_End
            | N_Unused_At_Start
            => return True;

         when N_Empty
            | N_Delay_Relative_Statement -- ????not turned into rt call?
            | N_Expression_Function
            | N_Iterated_Component_Association -- ????
            | N_Single_Protected_Declaration
            | N_Accept_Alternative -- ????not turned into rt call?
            | N_Accept_Statement -- ????not turned into rt call?
            | N_Decimal_Fixed_Point_Definition
            | N_Digits_Constraint
            | N_Entry_Call_Statement -- ????not turned into rt call?
            | N_Requeue_Statement -- ????not turned into rt call?
            | N_Selective_Accept -- ????not turned into rt call?
            | N_Terminate_Alternative -- ????not turned into rt call?
            | N_Defining_Character_Literal
            | N_Access_Function_Definition
            | N_Formal_Discrete_Type_Definition
            | N_Formal_Modular_Type_Definition
            | N_Iterator_Specification
            | N_Op_Expon
            | N_Variant
            | N_Variant_Part
            | N_Access_Definition
            | N_Access_Procedure_Definition
            | N_Access_To_Object_Definition
            | N_Aspect_Specification
            | N_Case_Statement_Alternative
            | N_Compilation_Unit_Aux
            | N_Component_Clause
            | N_Component_Declaration
            | N_Component_Definition
            | N_Component_List
            | N_Constrained_Array_Definition
            | N_Derived_Type_Definition
            | N_Designator
            | N_Discriminant_Association
            | N_Discriminant_Specification
            | N_Elsif_Part
            | N_Enumeration_Type_Definition
            | N_Floating_Point_Definition
            | N_Formal_Concrete_Subprogram_Declaration
            | N_Formal_Floating_Point_Definition
            | N_Formal_Object_Declaration
            | N_Formal_Private_Type_Definition
            | N_Formal_Signed_Integer_Type_Definition
            | N_Formal_Type_Declaration
            | N_Generic_Association
            | N_Index_Or_Discriminant_Constraint
            | N_Iteration_Scheme
            | N_Loop_Parameter_Specification
            | N_Modular_Type_Definition
            | N_Others_Choice
            | N_Parameter_Association
            | N_Parameter_Specification
            | N_Quantified_Expression -- ????
            | N_Range
            | N_Range_Constraint
            | N_Record_Definition
            | N_Signed_Integer_Type_Definition
            | N_Subtype_Indication
            | N_Unconstrained_Array_Definition
            | N_Pragma_Argument_Association
            | N_Case_Expression
            | N_Case_Expression_Alternative
            | N_Delta_Aggregate -- ????
            | N_Entry_Body_Formal_Part
            | N_Entry_Declaration
            | N_Extended_Return_Statement -- ????
            | N_Formal_Abstract_Subprogram_Declaration
            | N_Formal_Decimal_Fixed_Point_Definition
            | N_Formal_Incomplete_Type_Definition
            | N_Formal_Ordinary_Fixed_Point_Definition
            | N_Ordinary_Fixed_Point_Definition
            | N_Protected_Definition
            | N_Raise_Expression
            | N_Real_Range_Specification
            | N_Target_Name -- ????
            | N_Task_Definition
            => return False;
         --  ????

         when N_Abstract_Subprogram_Declaration
            | N_Aggregate
            | N_Allocator
            | N_And_Then
            | N_Assignment_Statement
            | N_At_Clause
            | N_Attribute_Definition_Clause
            | N_Attribute_Reference
            | N_Block_Statement
            | N_Call_Marker
            | N_Case_Statement
            | N_Character_Literal
            | N_Code_Statement
            | N_Compilation_Unit
            | N_Component_Association
            | N_Defining_Identifier
            | N_Defining_Operator_Symbol
            | N_Defining_Program_Unit_Name
            | N_Entry_Body
            | N_Enumeration_Representation_Clause
            | N_Exception_Declaration
            | N_Exception_Handler
            | N_Exception_Renaming_Declaration
            | N_Exit_Statement
            | N_Expanded_Name
            | N_Explicit_Dereference
            | N_Expression_With_Actions
            | N_Extension_Aggregate
            | N_External_Initializer
            | N_Free_Statement
            | N_Freeze_Entity
            | N_Freeze_Generic_Entity
            | N_Full_Type_Declaration
            | N_Function_Call
            | N_Function_Instantiation
            | N_Function_Specification
            | N_Generic_Function_Renaming_Declaration
            | N_Generic_Package_Declaration
            | N_Generic_Package_Renaming_Declaration
            | N_Generic_Procedure_Renaming_Declaration
            | N_Generic_Subprogram_Declaration
            | N_Goto_Statement
            | N_Handled_Sequence_Of_Statements
            | N_Identifier
            | N_If_Expression
            | N_If_Statement
            | N_Implicit_Label_Declaration
            | N_In
            | N_Incomplete_Type_Declaration
            | N_Indexed_Component
            | N_Integer_Literal
            | N_Itype_Reference
            | N_Label
            | N_Loop_Statement
            | N_Not_In
            | N_Null
            | N_Null_Statement
            | N_Number_Declaration
            | N_Object_Declaration
            | N_Object_Renaming_Declaration
            | N_Op_Abs
            | N_Op_Add
            | N_Op_And
            | N_Op_Concat
            | N_Op_Divide
            | N_Op_Eq
            | N_Operator_Symbol
            | N_Op_Ge
            | N_Op_Gt
            | N_Op_Le
            | N_Op_Lt
            | N_Op_Minus
            | N_Op_Mod
            | N_Op_Multiply
            | N_Op_Ne
            | N_Op_Not
            | N_Op_Or
            | N_Op_Plus
            | N_Op_Rem
            | N_Op_Rotate_Left
            | N_Op_Rotate_Right
            | N_Op_Shift_Left
            | N_Op_Shift_Right
            | N_Op_Shift_Right_Arithmetic
            | N_Op_Subtract
            | N_Op_Xor
            | N_Or_Else
            | N_Package_Body
            | N_Package_Body_Stub
            | N_Package_Declaration
            | N_Package_Instantiation
            | N_Package_Renaming_Declaration
            | N_Package_Specification
            | N_Pop_Constraint_Error_Label
            | N_Pop_Program_Error_Label
            | N_Pop_Storage_Error_Label
            | N_Pragma
            | N_Private_Extension_Declaration
            | N_Private_Type_Declaration
            | N_Procedure_Call_Statement
            | N_Procedure_Instantiation
            | N_Procedure_Specification
            | N_Protected_Body
            | N_Protected_Body_Stub
            | N_Protected_Type_Declaration
            | N_Push_Constraint_Error_Label
            | N_Push_Program_Error_Label
            | N_Push_Storage_Error_Label
            | N_Qualified_Expression
            | N_Raise_Constraint_Error
            | N_Raise_Program_Error
            | N_Raise_Statement
            | N_Raise_Storage_Error
            | N_Real_Literal
            | N_Record_Representation_Clause
            | N_Reference
            | N_Selected_Component
            | N_Simple_Return_Statement
            | N_Single_Task_Declaration
            | N_Slice
            | N_String_Literal
            | N_Subprogram_Body
            | N_Subprogram_Body_Stub
            | N_Subprogram_Declaration
            | N_Subprogram_Renaming_Declaration
            | N_Subtype_Declaration
            | N_Subunit
            | N_Task_Body
            | N_Task_Body_Stub
            | N_Task_Type_Declaration
            | N_Type_Conversion
            | N_Unchecked_Type_Conversion
            | N_Use_Package_Clause
            | N_Use_Type_Clause
            | N_Validate_Unchecked_Conversion
            | N_Variable_Reference_Marker
            | N_With_Clause
            => return False;
      end case;
   end Is_FE_Only;

end VAST;
