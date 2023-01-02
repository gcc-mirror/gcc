------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               E X P _ C G                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2010-2023, Free Software Foundation, Inc.         --
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

with Atree;          use Atree;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Elists;         use Elists;
with Exp_Dbug;       use Exp_Dbug;
with Exp_Tss;        use Exp_Tss;
with Lib;            use Lib;
with Namet;          use Namet;
with Opt;            use Opt;
with Output;         use Output;
with Sem_Aux;        use Sem_Aux;
with Sem_Disp;       use Sem_Disp;
with Sem_Type;       use Sem_Type;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Sinput;         use Sinput;
with Snames;         use Snames;
with System;         use System;
with Table;
with Uintp;          use Uintp;

package body Exp_CG is

   --  We duplicate here some declarations from packages Interfaces.C and
   --  Interfaces.C_Streams because adding their dependence to the frontend
   --  causes bootstrapping problems with old versions of the compiler.

   subtype FILEs is System.Address;
   --  Corresponds to the C type FILE*

   subtype C_chars is System.Address;
   --  Pointer to null-terminated array of characters

   function fputs (Strng : C_chars; Stream : FILEs) return Integer;
   pragma Import (C, fputs, "fputs");

   --  Import the file stream associated with the "ci" output file. Done to
   --  generate the output in the file created and left opened by routine
   --  toplev.c before calling gnat1drv.

   Callgraph_Info_File : FILEs;
   pragma Import (C, Callgraph_Info_File);

   package Call_Graph_Nodes is new Table.Table (
      Table_Component_Type => Node_Id,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 50,
      Table_Increment      => 100,
      Table_Name           => "Call_Graph_Nodes");
   --  This table records nodes associated with dispatching calls and tagged
   --  type declarations found in the main compilation unit. Used as an
   --  auxiliary storage because the call-graph output requires fully qualified
   --  names and they are not available until the backend is called.

   function Is_Predefined_Dispatching_Operation (E : Entity_Id) return Boolean;
   --  Determines if E is a predefined primitive operation.
   --  Note: This routine should replace the routine with the same name that is
   --  currently available in exp_disp because it extends its functionality to
   --  handle fully qualified names. It's actually in Sem_Util. ???

   function Slot_Number (Prim : Entity_Id) return Uint;
   --  Returns the slot number associated with Prim. For predefined primitives
   --  the slot is returned as a negative number.

   procedure Write_Output (Str : String);
   --  Used to print a line in the output file (this is used as the
   --  argument for a call to Set_Special_Output in package Output).

   procedure Write_Call_Info (Call : Node_Id);
   --  Subsidiary of Generate_CG_Output that generates the output associated
   --  with a dispatching call.

   procedure Write_Type_Info (Typ : Entity_Id);
   --  Subsidiary of Generate_CG_Output that generates the output associated
   --  with a tagged type declaration.

   ------------------------
   -- Generate_CG_Output --
   ------------------------

   procedure Generate_CG_Output is
      N : Node_Id;

   begin
      --  No output if the "ci" output file has not been previously opened
      --  by toplev.c

      if Callgraph_Info_File = Null_Address then
         return;
      end if;

      --  Setup write routine, create the output file and generate the output

      Set_Special_Output (Write_Output'Access);

      for J in Call_Graph_Nodes.First .. Call_Graph_Nodes.Last loop
         N := Call_Graph_Nodes.Table (J);

         --  No action needed for subprogram calls removed by the expander
         --  (for example, calls to ignored ghost entities).

         if Nkind (N) = N_Null_Statement then
            pragma Assert (Nkind (Original_Node (N)) in N_Subprogram_Call);
            null;

         elsif Nkind (N) in N_Subprogram_Call then
            Write_Call_Info (N);

         else pragma Assert (Nkind (N) = N_Defining_Identifier);

            --  The type may be a private untagged type whose completion is
            --  tagged, in which case we must use the full tagged view.

            if not Is_Tagged_Type (N) and then Is_Private_Type (N) then
               N := Full_View (N);
            end if;

            pragma Assert (Is_Tagged_Type (N));

            Write_Type_Info (N);
         end if;
      end loop;

      Cancel_Special_Output;
   end Generate_CG_Output;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Call_Graph_Nodes.Init;
   end Initialize;

   -----------------------------------------
   -- Is_Predefined_Dispatching_Operation --
   -----------------------------------------

   function Is_Predefined_Dispatching_Operation
     (E : Entity_Id) return Boolean
   is
      function Homonym_Suffix_Length (E : Entity_Id) return Natural;
      --  Returns the length of the homonym suffix corresponding to E.
      --  Note: This routine relies on the functionality provided by routines
      --  of Exp_Dbug. Further work needed here to decide if it should be
      --  located in that package???

      ---------------------------
      -- Homonym_Suffix_Length --
      ---------------------------

      function Homonym_Suffix_Length (E : Entity_Id) return Natural is
         Prefix_Length : constant := 2;
         --  Length of prefix "__"

         H  : Entity_Id;
         Nr : Nat := 1;

      begin
         if not Has_Homonym (E) then
            return 0;

         else
            H := Homonym (E);
            while Present (H) loop
               if Scope (H) = Scope (E) then
                  Nr := Nr + 1;
               end if;

               H := Homonym (H);
            end loop;

            if Nr = 1 then
               return 0;

            --  Prefix "__" followed by number

            else
               declare
                  Result : Natural := Prefix_Length + 1;

               begin
                  while Nr >= 10 loop
                     Result := Result + 1;
                     Nr := Nr / 10;
                  end loop;

                  return Result;
               end;
            end if;
         end if;
      end Homonym_Suffix_Length;

      --  Local variables

      Full_Name     : constant String := Get_Name_String (Chars (E));
      Suffix_Length : Natural;
      TSS_Name      : TSS_Name_Type;

   --  Start of processing for Is_Predefined_Dispatching_Operation

   begin
      if not Is_Dispatching_Operation (E) then
         return False;
      end if;

      --  Search for and strip suffix for body-nested package entities

      Suffix_Length := Homonym_Suffix_Length (E);
      for J in reverse Full_Name'First + 2 .. Full_Name'Last loop
         if Full_Name (J) = 'X' then

            --  Include the "X", "Xb", "Xn", ... in the part of the
            --  suffix to be removed.

            Suffix_Length := Suffix_Length + Full_Name'Last - J + 1;
            exit;
         end if;

         exit when Full_Name (J) /= 'b' and then Full_Name (J) /= 'n';
      end loop;

      --  Most predefined primitives have internally generated names. Equality
      --  must be treated differently; the predefined operation is recognized
      --  as a homogeneous binary operator that returns Boolean.

      if Full_Name'Length > TSS_Name_Type'Length then
         TSS_Name :=
           TSS_Name_Type
             (Full_Name
               (Full_Name'Last - TSS_Name'Length - Suffix_Length + 1
                  .. Full_Name'Last - Suffix_Length));

         if        TSS_Name = TSS_Stream_Read
           or else TSS_Name = TSS_Stream_Write
           or else TSS_Name = TSS_Stream_Input
           or else TSS_Name = TSS_Stream_Output
           or else TSS_Name = TSS_Put_Image
           or else TSS_Name = TSS_Deep_Adjust
           or else TSS_Name = TSS_Deep_Finalize
         then
            return True;

         elsif not Has_Fully_Qualified_Name (E) then
            if Chars (E) in Name_uSize | Name_uAlignment | Name_uAssign
              or else
                (Chars (E) = Name_Op_Eq
                  and then Etype (First_Formal (E)) = Etype (Last_Formal (E)))
              or else Is_Predefined_Interface_Primitive (E)
            then
               return True;
            end if;

         --  Handle fully qualified names

         else
            declare
               type Names_Table is array (Positive range <>) of Name_Id;

               Predef_Names_95 : constant Names_Table :=
                                   (Name_uSize,
                                    Name_uAlignment,
                                    Name_Op_Eq,
                                    Name_uAssign);

               Predef_Names_05 : constant Names_Table :=
                                   (Name_uDisp_Asynchronous_Select,
                                    Name_uDisp_Conditional_Select,
                                    Name_uDisp_Get_Prim_Op_Kind,
                                    Name_uDisp_Get_Task_Id,
                                    Name_uDisp_Requeue,
                                    Name_uDisp_Timed_Select);

            begin
               for J in Predef_Names_95'Range loop
                  Get_Name_String (Predef_Names_95 (J));

                  --  The predefined primitive operations are identified by the
                  --  names "_size", "_alignment", etc. If we try a pattern
                  --  matching against this string, we can wrongly match other
                  --  primitive operations like "get_size". To avoid this, we
                  --  add the "__" scope separator, which can only prepend
                  --  predefined primitive operations because other primitive
                  --  operations can neither start with an underline nor
                  --  contain two consecutive underlines in its name.

                  if Full_Name'Last - Suffix_Length > Name_Len + 2
                    and then
                      Full_Name
                        (Full_Name'Last - Name_Len - 2 - Suffix_Length + 1
                           .. Full_Name'Last - Suffix_Length) =
                      "__" & Name_Buffer (1 .. Name_Len)
                  then
                     --  For the equality operator the type of the two operands
                     --  must also match.

                     return Predef_Names_95 (J) /= Name_Op_Eq
                       or else
                         Etype (First_Formal (E)) = Etype (Last_Formal (E));
                  end if;
               end loop;

               if Ada_Version >= Ada_2005 then
                  for J in Predef_Names_05'Range loop
                     Get_Name_String (Predef_Names_05 (J));

                     if Full_Name'Last - Suffix_Length > Name_Len + 2
                       and then
                         Full_Name
                           (Full_Name'Last - Name_Len - 2 - Suffix_Length + 1
                              .. Full_Name'Last - Suffix_Length) =
                         "__" & Name_Buffer (1 .. Name_Len)
                     then
                        return True;
                     end if;
                  end loop;
               end if;
            end;
         end if;
      end if;

      return False;
   end Is_Predefined_Dispatching_Operation;

   ----------------------
   -- Register_CG_Node --
   ----------------------

   procedure Register_CG_Node (N : Node_Id) is
   begin
      if Nkind (N) in N_Subprogram_Call then
         if Current_Scope = Main_Unit_Entity
           or else Entity_Is_In_Main_Unit (Current_Scope)
         then
            --  Register a copy of the dispatching call node. Needed since the
            --  node containing a dispatching call is rewritten by the
            --  expander.

            declare
               Copy : constant Node_Id := New_Copy (N);
               Par  : Node_Id;

            begin
               --  Determine the enclosing scope to use when generating the
               --  call graph. This must be done now to avoid problems with
               --  control structures that may be rewritten during expansion.

               Par := Parent (N);
               while Nkind (Par) /= N_Subprogram_Body
                 and then Nkind (Parent (Par)) /= N_Compilation_Unit
               loop
                  Par := Parent (Par);

                  --  Par can legitimately be empty inside a class-wide
                  --  precondition; the "real" call will be found inside the
                  --  generated pragma.

                  if No (Par) then
                     return;
                  end if;
               end loop;

               Set_Parent (Copy, Par);
               Call_Graph_Nodes.Append (Copy);
            end;
         end if;

      else pragma Assert (Nkind (N) = N_Defining_Identifier);
         if Entity_Is_In_Main_Unit (N) then
            Call_Graph_Nodes.Append (N);
         end if;
      end if;
   end Register_CG_Node;

   -----------------
   -- Slot_Number --
   -----------------

   function Slot_Number (Prim : Entity_Id) return Uint is
      E : constant Entity_Id := Ultimate_Alias (Prim);
   begin
      if Is_Predefined_Dispatching_Operation (E) then
         return -DT_Position (E);
      else
         return DT_Position (E);
      end if;
   end Slot_Number;

   ------------------
   -- Write_Output --
   ------------------

   procedure Write_Output (Str : String) is
      Nul   : constant Character := Character'First;
      Line  : String (Str'First .. Str'Last + 1);
      Errno : Integer;

   begin
      --  Add the null character to the string as required by fputs

      Line  := Str & Nul;
      Errno := fputs (Line'Address, Callgraph_Info_File);
      pragma Assert (Errno >= 0);
   end Write_Output;

   ---------------------
   -- Write_Call_Info --
   ---------------------

   procedure Write_Call_Info (Call : Node_Id) is
      Ctrl_Arg : constant Node_Id   := Controlling_Argument (Call);
      Ctrl_Typ : constant Entity_Id := Base_Type (Etype (Ctrl_Arg));
      Prim     : constant Entity_Id := Entity (Sinfo.Nodes.Name (Call));
      P        : constant Node_Id   := Parent (Call);

   begin
      Write_Str ("edge: { sourcename: ");
      Write_Char ('"');

      --  The parent node is the construct that contains the call: subprogram
      --  body or library-level package. Display the qualified name of the
      --  entity of the construct. For a subprogram, it is the entity of the
      --  spec, which carries a homonym counter when it is overloaded.

      if Nkind (P) = N_Subprogram_Body
        and then not Acts_As_Spec (P)
      then
         Get_External_Name (Corresponding_Spec (P));

      else
         Get_External_Name (Defining_Entity (P));
      end if;

      Write_Str (Name_Buffer (1 .. Name_Len));

      if Nkind (P) = N_Package_Declaration then
         Write_Str ("___elabs");

      elsif Nkind (P) = N_Package_Body then
         Write_Str ("___elabb");
      end if;

      Write_Char ('"');
      Write_Eol;

      --  The targetname is a triple:
      --     N:  the index in a vtable used for dispatch
      --     V:  the type who's vtable is used
      --     S:  the static type of the expression

      Write_Str  ("  targetname: ");
      Write_Char ('"');

      pragma Assert (No (Interface_Alias (Prim)));

      --  The check on Is_Ancestor is done here to avoid problems with
      --  renamings of primitives. For example:

      --    type Root is tagged ...
      --    procedure Base   (Obj : Root);
      --    procedure Base2  (Obj : Root) renames Base;

      if Present (Alias (Prim))
        and then
          Is_Ancestor
            (Find_Dispatching_Type (Ultimate_Alias (Prim)),
             Root_Type (Ctrl_Typ),
             Use_Full_View => True)
      then
         --  This is a special case in which we generate in the ci file the
         --  slot number of the renaming primitive (i.e. Base2) but instead of
         --  generating the name of this renaming entity we reference directly
         --  the renamed entity (i.e. Base).

         Write_Int (UI_To_Int (Slot_Number (Prim)));
         Write_Char (':');
         Write_Name
           (Chars (Find_Dispatching_Type (Ultimate_Alias (Prim))));
      else
         Write_Int (UI_To_Int (Slot_Number (Prim)));
         Write_Char (':');
         Write_Name (Chars (Root_Type (Ctrl_Typ)));
      end if;

      Write_Char (',');
      Write_Name (Chars (Root_Type (Ctrl_Typ)));

      Write_Char ('"');
      Write_Eol;

      Write_Str  ("  label: ");
      Write_Char ('"');
      Write_Location (Sloc (Call));
      Write_Char ('"');
      Write_Eol;

      Write_Char ('}');
      Write_Eol;
   end Write_Call_Info;

   ---------------------
   -- Write_Type_Info --
   ---------------------

   procedure Write_Type_Info (Typ : Entity_Id) is
      Elmt : Elmt_Id;
      Prim : Node_Id;

      Parent_Typ       : Entity_Id;
      Separator_Needed : Boolean := False;

   begin
      --  Initialize Parent_Typ handling private types

      Parent_Typ := Etype (Typ);

      if Present (Full_View (Parent_Typ)) then
         Parent_Typ := Full_View (Parent_Typ);
      end if;

      Write_Str ("class {");
      Write_Eol;

      Write_Str ("  classname: ");
      Write_Char ('"');
      Write_Name (Chars (Typ));
      Write_Char ('"');
      Write_Eol;

      Write_Str  ("  label: ");
      Write_Char ('"');
      Write_Name (Chars (Typ));
      Write_Char ('\');
      Write_Location (Sloc (Typ));
      Write_Char ('"');
      Write_Eol;

      if Parent_Typ /= Typ then
         Write_Str  ("  parent: ");
         Write_Char ('"');
         Write_Name (Chars (Parent_Typ));

         --  Note: Einfo.Entities prefix not needed if this routine is moved to
         --  exp_disp???

         if Present (Einfo.Entities.Interfaces (Typ))
           and then not Is_Empty_Elmt_List (Einfo.Entities.Interfaces (Typ))
         then
            Elmt := First_Elmt (Einfo.Entities.Interfaces (Typ));
            while Present (Elmt) loop
               Write_Str  (", ");
               Write_Name (Chars (Node (Elmt)));
               Next_Elmt  (Elmt);
            end loop;
         end if;

         Write_Char ('"');
         Write_Eol;
      end if;

      Write_Str ("  virtuals: ");
      Write_Char ('"');

      Elmt := First_Elmt (Primitive_Operations (Typ));
      while Present (Elmt) loop
         Prim := Node (Elmt);

         --  Skip internal entities associated with overridden interface
         --  primitives, and also inherited primitives.

         if Present (Interface_Alias (Prim))
           or else
             (Present (Alias (Prim))
               and then Find_Dispatching_Type (Prim) /=
                        Find_Dispatching_Type (Alias (Prim)))
         then
            goto Continue;
         end if;

         --  Do not generate separator for output of first primitive

         if Separator_Needed then
            Write_Str ("\n");
            Write_Eol;
            Write_Str ("             ");
         else
            Separator_Needed := True;
         end if;

         Write_Int (UI_To_Int (Slot_Number (Prim)));
         Write_Char (':');

         --  Handle renamed primitives

         if Present (Alias (Prim)) then
            Write_Name (Chars (Ultimate_Alias (Prim)));
         else
            Write_Name (Chars (Prim));
         end if;

         --  Display overriding of parent primitives

         if Present (Overridden_Operation (Prim))
           and then
             Is_Ancestor
               (Find_Dispatching_Type (Overridden_Operation (Prim)), Typ,
                Use_Full_View => True)
         then
            Write_Char (',');
            Write_Int
              (UI_To_Int (Slot_Number (Overridden_Operation (Prim))));
            Write_Char (':');
            Write_Name
              (Chars (Find_Dispatching_Type (Overridden_Operation (Prim))));
         end if;

         --  Display overriding of interface primitives

         if Has_Interfaces (Typ) then
            declare
               Prim_Elmt : Elmt_Id;
               Prim_Op   : Node_Id;
               Int_Alias : Entity_Id;

            begin
               Prim_Elmt := First_Elmt (Primitive_Operations (Typ));
               while Present (Prim_Elmt) loop
                  Prim_Op := Node (Prim_Elmt);
                  Int_Alias := Interface_Alias (Prim_Op);

                  if Present (Int_Alias)
                    and then
                      not Is_Ancestor (Find_Dispatching_Type (Int_Alias), Typ,
                                       Use_Full_View => True)
                    and then (Alias (Prim_Op)) = Prim
                  then
                     Write_Char (',');
                     Write_Int (UI_To_Int (Slot_Number (Int_Alias)));
                     Write_Char (':');
                     Write_Name (Chars (Find_Dispatching_Type (Int_Alias)));
                  end if;

                  Next_Elmt (Prim_Elmt);
               end loop;
            end;
         end if;

         <<Continue>>
         Next_Elmt (Elmt);
      end loop;

      Write_Char ('"');
      Write_Eol;

      Write_Char ('}');
      Write_Eol;
   end Write_Type_Info;

end Exp_CG;
