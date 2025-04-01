------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 V A S T                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2020-2025, Free Software Foundation, Inc.      --
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
with Lib;            use Lib;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Opt;            use Opt;
with Output;
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
      Check_Sharing,
      Check_Parent_Present,
      Check_Parent_Correct);

   type Check_Status is
     --  Action in case of check failure:
     (Disabled, -- Do nothing
      Enabled, -- Print messages, and raise an exception
      Print_And_Continue); -- Print a message

   pragma Warnings (Off, "Status*could be declared constant");
   Status : array (Check_Enum) of Check_Status :=
     (Check_Other => Enabled,
      Check_Sloc => Disabled,
      Check_Analyzed => Disabled,
      Check_Error_Nodes => Print_And_Continue,
      Check_Sharing => Disabled,
      Check_Parent_Present => Print_And_Continue,
      Check_Parent_Correct => Disabled);
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

   procedure Assert
     (Condition : Boolean;
      Check : Check_Enum := Check_Other;
      Detail : String := "");
   --  Check that the Condition is True. Status determines action on failure.

   function To_Mixed (A : String) return String;
   --  Copied from System.Case_Util; old versions of that package do not have
   --  this function, so this is needed for bootstrapping.

   function Image (Kind : Node_Kind) return String is (To_Mixed (Kind'Img));
   function Image (Kind : Entity_Kind) return String is (To_Mixed (Kind'Img));

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

   function Has_Subtrees (N : Node_Id) return Boolean;
   --  True if N has one or more syntactic fields

   procedure Do_Subtrees (N : Node_Id);
   --  Call Do_Tree on all the subtrees (i.e. syntactic fields) of N

   procedure Do_List (L : List_Id);
   --  Call Do_Tree on the list elements

   procedure Do_Unit (U : Unit_Number_Type);
   --  Call Do_Tree on the root node of a compilation unit

   function Ancestor_Node (Count : Node_Stack_Count) return Node_Id;
   --  Nth ancestor on the Node_Stack. Ancestor_Node(0) is the current node,
   --  Ancestor_Node(1) is its parent, Ancestor_Node(2) is its grandparent,
   --  and so on.

   function Top_Node return Node_Id is (Ancestor_Node (0));

   type Node_Set is array (Node_Id range <>) of Boolean;
   pragma Pack (Node_Set);
   type Node_Set_Ptr is access all Node_Set;
   procedure Free is new Ada.Unchecked_Deallocation (Node_Set, Node_Set_Ptr);

   Visited : Node_Set_Ptr;
   --  Giant array of Booleans; Visited (N) is True if and only if we have
   --  visited N in the tree walk. Used to detect incorrect sharing of subtrees
   --  or (worse) cycles. We don't allocate the set on the stack, for fear of
   --  Storage_Error.

   function Get_Node_Field_Union is new
     Atree.Atree_Private_Part.Get_32_Bit_Field (Union_Id) with Inline;

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

   --------------
   -- Put_Node --
   --------------

   procedure Put_Node (N : Node_Id) is
   begin
      if Debug.Debug_Flag_Underscore_WW then
         if Nkind (N) in N_Entity then
            Put (Image (Ekind (N)));
         else
            Put (Image (Nkind (N)));
         end if;

         Put (N'Img & " ");
         Sinput.Write_Location (Sloc (N));

         if Comes_From_Source (N) then
            Put (" (s)");
         end if;

         case Nkind (N) is
            when N_Has_Chars =>
               Put (" ");
               Write_Name_For_Debug (Chars (N), Quote => """");
            when others => null;
         end case;

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

   -------------------
   -- Ancestor_Node --
   -------------------

   function Ancestor_Node (Count : Node_Stack_Count) return Node_Id is
   begin
      return Node_Stack.Table (Node_Stack.Last - Count);
   end Ancestor_Node;

   ------------
   -- Assert --
   ------------

   VAST_Failure : exception;

   procedure Assert
     (Condition : Boolean;
      Check : Check_Enum := Check_Other;
      Detail : String := "")
   is
   begin
      if not Condition then
         declare
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
                  --  ???We should probably avoid changing the debug flag here
                  Put (Message & ": ");
                  Put_Node (Top_Node);
                  Put_Line ("");

                  if Status (Check) = Enabled then
                     Put_Node_Stack;
                     raise VAST_Failure with Message;
                  end if;

                  Debug.Debug_Flag_Underscore_WW := Save;
            end case;
         end;
      end if;
   end Assert;

   -------------
   -- Do_Tree --
   -------------

   procedure Do_Tree (N : Node_Id) is
   begin
      Enter_Node (N);

      --  Skip the rest if empty. Check Sloc:

      case Nkind (N) is
         when N_Empty =>
            Assert (No (Sloc (N)));
            goto Done; -- -------------->
            --  Don't do any further checks on Empty

         --  ???Some nodes, including exception handlers, have no Sloc;
         --  it's unclear why.

         when N_Exception_Handler =>
            Assert (if Comes_From_Source (N) then Present (Sloc (N)));
         when others =>
            Assert (Present (Sloc (N)), Check_Sloc);
      end case;

      --  All reachable nodes should have been analyzed by the time we get
      --  here:

      Assert (Analyzed (N), Check_Analyzed);

      --  If we visit the same node more than once, then there are shared
      --  nodes; the "tree" is not a tree:

      Assert (not Visited (N), Check_Sharing);
      Visited (N) := True;

      --  Misc checks based on node/entity kind:

      case Nkind (N) is
         when N_Unused_At_Start | N_Unused_At_End =>
            Assert (False);

         when N_Error =>
            --  VAST doesn't do anything when Serious_Errors_Detected > 0 (at
            --  least for now), so we shouldn't encounter any N_Error nodes.
            Assert (False, Check_Error_Nodes);

         when N_Entity =>
            case Ekind (N) is
               when others =>
                  null; -- more to be done here
            end case;

         when others =>
            null; -- more to be done here
      end case;

      --  Check that N has a Parent, except in certain cases:

      case Nkind (N) is
         when N_Empty =>
            raise Program_Error; -- can't get here

         when N_Error =>
            Assert (False, Check_Error_Nodes);
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

      Do_Subtrees (N);

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
      --  True if sentinel comes first
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

      Is_Preprocessing_Dependency : constant Boolean :=
        U_Name = No_Unit_Name;
      --  True if this is a bogus unit added by Add_Preprocessing_Dependency.
      --  ???Not sure what that's about, but these units have no name and
      --  no associated tree, so we had better not try to walk those trees.

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
      pragma Assert (Expander_Active = (Operating_Mode = Generate_Code));
      --  ???So why do we need both Operating_Mode and Expander_Active?
      use Debug;
   begin
      --  Do nothing if we're not calling the back end; the main point of VAST
      --  is to protect against code-generation bugs. This includes the
      --  case where legality errors were detected; the tree is known to be
      --  malformed in some error cases.

      if Operating_Mode /= Generate_Code then
         return;
      end if;

      --  If -gnatd_W (VAST in verbose mode) is enabled, then that should imply
      --  -gnatd_V (enable VAST).

      if Debug_Flag_Underscore_WW then
         Debug_Flag_Underscore_VV := True;
      end if;

      --  Do nothing if VAST is disabled

      if not (Debug_Flag_Underscore_VV or Force_Enable_VAST) then
         return;
      end if;

      --  Turn off output unless verbose mode is enabled

      Put_Line ("VAST");

      --  Operating_Mode = Generate_Code implies there are no legality errors:

      Assert (Serious_Errors_Detected = 0);

      Put_Line ("VAST checking" & Last_Unit'Img & " units");

      declare
         use Atree_Private_Part;
         Last_Node : constant Node_Id := Node_Offsets.Last;
      begin
         pragma Assert (Visited = null);
         Visited := new Node_Set'(Node_Id'First .. Last_Node => False);

         for U in Main_Unit .. Last_Unit loop
            --  Main_Unit is the one passed to the back end, but here we are
            --  walking all the units.
            Do_Unit (U);
         end loop;

         --  We shouldn't have allocated any new nodes during VAST:

         pragma Assert (Node_Offsets.Last = Last_Node);
         Free (Visited);
      end;

      Put_Line ("VAST done.");
   end VAST;

end VAST;
