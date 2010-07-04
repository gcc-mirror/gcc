------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               E X P _ C G                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2010, Free Software Foundation, Inc.           --
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

with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Exp_Disp; use Exp_Disp;
with Exp_Dbug; use Exp_Dbug;
with Exp_Tss;  use Exp_Tss;
with Lib;      use Lib;
with Namet;    use Namet;
with Opt;      use Opt;
with Output;   use Output;
with Sem_Aux;  use Sem_Aux;
with Sem_Disp; use Sem_Disp;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with System;   use System;
with Table;
with Uintp;    use Uintp;

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
   --  handle fully qualified names ???

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
      --  by toplev.c. Temporarily the output is also disabled with -gnatd.Z

      if Callgraph_Info_File = Null_Address
        or else not Debug_Flag_Dot_ZZ
      then
         return;
      end if;

      --  Setup write routine, create the output file and generate the output

      Set_Special_Output (Write_Output'Access);

      for J in Call_Graph_Nodes.First .. Call_Graph_Nodes.Last loop
         N := Call_Graph_Nodes.Table (J);

         if Nkind_In (N, N_Procedure_Call_Statement, N_Function_Call) then
            Write_Call_Info (N);

         else pragma Assert (Nkind (N) = N_Defining_Identifier);
            pragma Assert (Is_Tagged_Type (N));

            Write_Type_Info (N);
         end if;
      end loop;

      Set_Special_Output (null);
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
         Prefix_Length : constant := 2; --  Length of prefix "__"

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
                  while Nr > 10 loop
                     Result := Result + 1;
                     Nr := Nr / 10;
                  end loop;
                  return Result;
               end;
            end if;
         end if;
      end Homonym_Suffix_Length;

      --  Local variables

      Full_Name : constant String := Get_Name_String (Chars (E));
      TSS_Name  : TSS_Name_Type;

   --  Start of processing for Is_Predefined_Dispatching_Operation

   begin
      if not Is_Dispatching_Operation (E) then
         return False;
      end if;

      --  Most predefined primitives have internally generated names. Equality
      --  must be treated differently; the predefined operation is recognized
      --  as a homogeneous binary operator that returns Boolean.

      if Full_Name'Length > TSS_Name_Type'Length then
         TSS_Name :=
           TSS_Name_Type (Full_Name (Full_Name'Last - TSS_Name'Length + 1
                           .. Full_Name'Last));

         if        TSS_Name = TSS_Stream_Read
           or else TSS_Name = TSS_Stream_Write
           or else TSS_Name = TSS_Stream_Input
           or else TSS_Name = TSS_Stream_Output
           or else TSS_Name = TSS_Deep_Adjust
           or else TSS_Name = TSS_Deep_Finalize
         then
            return True;

         elsif not Has_Fully_Qualified_Name (E) then
            if        Chars (E) = Name_uSize
              or else Chars (E) = Name_uAlignment
              or else
                (Chars (E) = Name_Op_Eq
                   and then Etype (First_Formal (E)) = Etype (Last_Formal (E)))
              or else Chars (E) = Name_uAssign
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

               Suffix_Length : constant Natural := Homonym_Suffix_Length (E);

            begin
               for J in Predef_Names_95'Range loop
                  Get_Name_String (Predef_Names_95 (J));

                  if Full_Name'Last - Suffix_Length > Name_Len
                    and then
                      Full_Name
                        (Full_Name'Last - Name_Len - Suffix_Length + 1
                           .. Full_Name'Last - Suffix_Length) =
                                                  Name_Buffer (1 .. Name_Len)
                  then
                     --  For the equality operator the type of the two operands
                     --  must also match.

                     return Predef_Names_95 (J) /= Name_Op_Eq
                       or else
                         Etype (First_Formal (E)) = Etype (Last_Formal (E));
                  end if;
               end loop;

               if Ada_Version >= Ada_05 then
                  for J in Predef_Names_05'Range loop
                     Get_Name_String (Predef_Names_05 (J));

                     if Full_Name'Last - Suffix_Length > Name_Len
                       and then
                         Full_Name
                           (Full_Name'Last - Name_Len - Suffix_Length + 1
                              .. Full_Name'Last - Suffix_Length) =
                                                 Name_Buffer (1 .. Name_Len)
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
      if Nkind_In (N, N_Procedure_Call_Statement, N_Function_Call) then
         if Current_Scope = Main_Unit_Entity
           or else Entity_Is_In_Main_Unit (Current_Scope)
         then
            --  Register a copy of the dispatching call node. Needed since the
            --  node containing a dispatching call is rewriten by the expander.

            declare
               Copy : constant Node_Id := New_Copy (N);

            begin
               --  Copy the link to the parent to allow climbing up the tree
               --  when the call-graph information is generated

               Set_Parent (Copy, Parent (N));
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
   begin
      if Is_Predefined_Dispatching_Operation (Prim) then
         return -DT_Position (Prim);
      else
         return DT_Position (Prim);
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
      Prim     : constant Entity_Id := Entity (Sinfo.Name (Call));
      P        : Node_Id;

   begin
      --  Locate the enclosing context: a subprogram (if available) or the
      --  enclosing library-level package

      P := Parent (Call);
      while Nkind (P) /= N_Subprogram_Body
        and then Nkind (Parent (P)) /= N_Compilation_Unit
      loop
         P := Parent (P);
         pragma Assert (Present (P));
      end loop;

      Write_Str ("edge: { sourcename: ");
      Write_Char ('"');
      Get_External_Name (Defining_Entity (P), Has_Suffix => False);
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
             Root_Type (Ctrl_Typ))
      then
         Write_Int (UI_To_Int (Slot_Number (Ultimate_Alias (Prim))));
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

         --  Note: Einfo prefix not needed if this routine is moved to
         --  exp_disp???

         if Present (Einfo.Interfaces (Typ))
           and then not Is_Empty_Elmt_List (Einfo.Interfaces (Typ))
         then
            Elmt := First_Elmt (Einfo.Interfaces (Typ));
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

         --  Display only primitives overriden or defined

         if Present (Alias (Prim)) then
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
         Write_Name (Chars (Prim));

         --  Display overriding of parent primitives

         if Present (Overridden_Operation (Prim))
           and then
             Is_Ancestor
               (Find_Dispatching_Type (Overridden_Operation (Prim)), Typ)
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
                    and then not Is_Ancestor
                                   (Find_Dispatching_Type (Int_Alias), Typ)
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
