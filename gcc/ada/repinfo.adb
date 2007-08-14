------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              R E P I N F O                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2007, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Alloc;  use Alloc;
with Atree;  use Atree;
with Casing; use Casing;
with Debug;  use Debug;
with Einfo;  use Einfo;
with Lib;    use Lib;
with Namet;  use Namet;
with Opt;    use Opt;
with Output; use Output;
with Sinfo;  use Sinfo;
with Sinput; use Sinput;
with Snames; use Snames;
with Stand;  use Stand;
with Table;  use Table;
with Uname;  use Uname;
with Urealp; use Urealp;

with Ada.Unchecked_Conversion;

package body Repinfo is

   SSU : constant := 8;
   --  Value for Storage_Unit, we do not want to get this from TTypes, since
   --  this introduces problematic dependencies in ASIS, and in any case this
   --  value is assumed to be 8 for the implementation of the DDA.

   --  This is wrong for AAMP???

   ---------------------------------------
   -- Representation of gcc Expressions --
   ---------------------------------------

   --    This table is used only if Frontend_Layout_On_Target is False, so gigi
   --    lays out dynamic size/offset fields using encoded gcc expressions.

   --    A table internal to this unit is used to hold the values of back
   --    annotated expressions. This table is written out by -gnatt and read
   --    back in for ASIS processing.

   --    Node values are stored as Uint values using the negative of the node
   --    index in this table. Constants appear as non-negative Uint values.

   type Exp_Node is record
      Expr : TCode;
      Op1  : Node_Ref_Or_Val;
      Op2  : Node_Ref_Or_Val;
      Op3  : Node_Ref_Or_Val;
   end record;

   --  The following representation clause ensures that the above record
   --  has no holes. We do this so that when instances of this record are
   --  written by Tree_Gen, we do not write uninitialized values to the file.

   for Exp_Node use record
      Expr at  0 range 0 .. 31;
      Op1  at  4 range 0 .. 31;
      Op2  at  8 range 0 .. 31;
      Op3  at 12 range 0 .. 31;
   end record;

   for Exp_Node'Size use 16 * 8;
   --  This ensures that we did not leave out any fields

   package Rep_Table is new Table.Table (
      Table_Component_Type => Exp_Node,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => Alloc.Rep_Table_Initial,
      Table_Increment      => Alloc.Rep_Table_Increment,
      Table_Name           => "BE_Rep_Table");

   --------------------------------------------------------------
   -- Representation of Front-End Dynamic Size/Offset Entities --
   --------------------------------------------------------------

   package Dynamic_SO_Entity_Table is new Table.Table (
      Table_Component_Type => Entity_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => Alloc.Rep_Table_Initial,
      Table_Increment      => Alloc.Rep_Table_Increment,
      Table_Name           => "FE_Rep_Table");

   Unit_Casing : Casing_Type;
   --  Identifier casing for current unit

   Need_Blank_Line : Boolean;
   --  Set True if a blank line is needed before outputting any information for
   --  the current entity. Set True when a new entity is processed, and false
   --  when the blank line is output.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Back_End_Layout return Boolean;
   --  Test for layout mode, True = back end, False = front end. This function
   --  is used rather than checking the configuration parameter because we do
   --  not want Repinfo to depend on Targparm (for ASIS)

   procedure Blank_Line;
   --  Called before outputting anything for an entity. Ensures that
   --  a blank line precedes the output for a particular entity.

   procedure List_Entities (Ent : Entity_Id);
   --  This procedure lists the entities associated with the entity E, starting
   --  with the First_Entity and using the Next_Entity link. If a nested
   --  package is found, entities within the package are recursively processed.

   procedure List_Name (Ent : Entity_Id);
   --  List name of entity Ent in appropriate case. The name is listed with
   --  full qualification up to but not including the compilation unit name.

   procedure List_Array_Info (Ent : Entity_Id);
   --  List representation info for array type Ent

   procedure List_Mechanisms (Ent : Entity_Id);
   --  List mechanism information for parameters of Ent, which is subprogram,
   --  subprogram type, or an entry or entry family.

   procedure List_Object_Info (Ent : Entity_Id);
   --  List representation info for object Ent

   procedure List_Record_Info (Ent : Entity_Id);
   --  List representation info for record type Ent

   procedure List_Type_Info (Ent : Entity_Id);
   --  List type info for type Ent

   function Rep_Not_Constant (Val : Node_Ref_Or_Val) return Boolean;
   --  Returns True if Val represents a variable value, and False if it
   --  represents a value that is fixed at compile time.

   procedure Spaces (N : Natural);
   --  Output given number of spaces

   procedure Write_Info_Line (S : String);
   --  Routine to write a line to Repinfo output file. This routine is passed
   --  as a special output procedure to Output.Set_Special_Output. Note that
   --  Write_Info_Line is called with an EOL character at the end of each line,
   --  as per the Output spec, but the internal call to the appropriate routine
   --  in Osint requires that the end of line sequence be stripped off.

   procedure Write_Mechanism (M : Mechanism_Type);
   --  Writes symbolic string for mechanism represented by M

   procedure Write_Val (Val : Node_Ref_Or_Val; Paren : Boolean := False);
   --  Given a representation value, write it out. No_Uint values or values
   --  dependent on discriminants are written as two question marks. If the
   --  flag Paren is set, then the output is surrounded in parentheses if it is
   --  other than a simple value.

   ---------------------
   -- Back_End_Layout --
   ---------------------

   function Back_End_Layout return Boolean is
   begin
      --  We have back end layout if the back end has made any entries in the
      --  table of GCC expressions, otherwise we have front end layout.

      return Rep_Table.Last > 0;
   end Back_End_Layout;

   ----------------
   -- Blank_Line --
   ----------------

   procedure Blank_Line is
   begin
      if Need_Blank_Line then
         Write_Eol;
         Need_Blank_Line := False;
      end if;
   end Blank_Line;

   ------------------------
   -- Create_Discrim_Ref --
   ------------------------

   function Create_Discrim_Ref (Discr : Entity_Id) return Node_Ref is
   begin
      return Create_Node
        (Expr => Discrim_Val,
         Op1  => Discriminant_Number (Discr));
   end Create_Discrim_Ref;

   ---------------------------
   -- Create_Dynamic_SO_Ref --
   ---------------------------

   function Create_Dynamic_SO_Ref (E : Entity_Id) return Dynamic_SO_Ref is
   begin
      Dynamic_SO_Entity_Table.Append (E);
      return UI_From_Int (-Dynamic_SO_Entity_Table.Last);
   end Create_Dynamic_SO_Ref;

   -----------------
   -- Create_Node --
   -----------------

   function Create_Node
     (Expr : TCode;
      Op1  : Node_Ref_Or_Val;
      Op2  : Node_Ref_Or_Val := No_Uint;
      Op3  : Node_Ref_Or_Val := No_Uint) return Node_Ref
   is
   begin
      Rep_Table.Append (
        (Expr => Expr,
         Op1  => Op1,
         Op2  => Op2,
         Op3  => Op3));
      return UI_From_Int (-Rep_Table.Last);
   end Create_Node;

   ---------------------------
   -- Get_Dynamic_SO_Entity --
   ---------------------------

   function Get_Dynamic_SO_Entity (U : Dynamic_SO_Ref) return Entity_Id is
   begin
      return Dynamic_SO_Entity_Table.Table (-UI_To_Int (U));
   end Get_Dynamic_SO_Entity;

   -----------------------
   -- Is_Dynamic_SO_Ref --
   -----------------------

   function Is_Dynamic_SO_Ref (U : SO_Ref) return Boolean is
   begin
      return U < Uint_0;
   end Is_Dynamic_SO_Ref;

   ----------------------
   -- Is_Static_SO_Ref --
   ----------------------

   function Is_Static_SO_Ref (U : SO_Ref) return Boolean is
   begin
      return U >= Uint_0;
   end Is_Static_SO_Ref;

   ---------
   -- lgx --
   ---------

   procedure lgx (U : Node_Ref_Or_Val) is
   begin
      List_GCC_Expression (U);
      Write_Eol;
   end lgx;

   ----------------------
   -- List_Array_Info --
   ----------------------

   procedure List_Array_Info (Ent : Entity_Id) is
   begin
      List_Type_Info (Ent);
      Write_Str ("for ");
      List_Name (Ent);
      Write_Str ("'Component_Size use ");
      Write_Val (Component_Size (Ent));
      Write_Line (";");
   end List_Array_Info;

   -------------------
   -- List_Entities --
   -------------------

   procedure List_Entities (Ent : Entity_Id) is
      Body_E : Entity_Id;
      E      : Entity_Id;

      function Find_Declaration (E : Entity_Id) return Node_Id;
      --  Utility to retrieve declaration node for entity in the
      --  case of package bodies and subprograms.

      ----------------------
      -- Find_Declaration --
      ----------------------

      function Find_Declaration (E : Entity_Id) return Node_Id is
         Decl : Node_Id;

      begin
         Decl := Parent (E);
         while Present (Decl)
           and then  Nkind (Decl) /= N_Package_Body
           and then Nkind (Decl) /= N_Subprogram_Declaration
           and then Nkind (Decl) /= N_Subprogram_Body
         loop
            Decl := Parent (Decl);
         end loop;

         return Decl;
      end Find_Declaration;

   --  Start of processing for List_Entities

   begin
      --  List entity if we have one, and it is not a renaming declaration.
      --  For renamings, we don't get proper information, and really it makes
      --  sense to restrict the output to the renamed entity.

      if Present (Ent)
        and then Nkind (Declaration_Node (Ent)) not in N_Renaming_Declaration
      then
         --  If entity is a subprogram and we are listing mechanisms,
         --  then we need to list mechanisms for this entity.

         if List_Representation_Info_Mechanisms
           and then (Is_Subprogram (Ent)
                       or else Ekind (Ent) = E_Entry
                       or else Ekind (Ent) = E_Entry_Family)
         then
            Need_Blank_Line := True;
            List_Mechanisms (Ent);
         end if;

         E := First_Entity (Ent);
         while Present (E) loop
            Need_Blank_Line := True;

            --  We list entities that come from source (excluding private or
            --  incomplete types or deferred constants, where we will list the
            --  info for the full view). If debug flag A is set, then all
            --  entities are listed

            if (Comes_From_Source (E)
              and then not Is_Incomplete_Or_Private_Type (E)
              and then not (Ekind (E) = E_Constant
                              and then Present (Full_View (E))))
              or else Debug_Flag_AA
            then
               if Is_Subprogram (E)
                       or else
                     Ekind (E) = E_Entry
                       or else
                     Ekind (E) = E_Entry_Family
                       or else
                     Ekind (E) = E_Subprogram_Type
               then
                  if List_Representation_Info_Mechanisms then
                     List_Mechanisms (E);
                  end if;

               elsif Is_Record_Type (E) then
                  if List_Representation_Info >= 1 then
                     List_Record_Info (E);
                  end if;

               elsif Is_Array_Type (E) then
                  if List_Representation_Info >= 1 then
                     List_Array_Info (E);
                  end if;

               elsif Is_Type (E) then
                  if List_Representation_Info >= 2 then
                     List_Type_Info (E);
                  end if;

               elsif Ekind (E) = E_Variable
                       or else
                     Ekind (E) = E_Constant
                       or else
                     Ekind (E) = E_Loop_Parameter
                       or else
                     Is_Formal (E)
               then
                  if List_Representation_Info >= 2 then
                     List_Object_Info (E);
                  end if;

               end if;

               --  Recurse into nested package, but not if they are package
               --  renamings (in particular renamings of the enclosing package,
               --  as for some Java bindings and for generic instances).

               if Ekind (E) = E_Package then
                  if No (Renamed_Object (E)) then
                     List_Entities (E);
                  end if;

               --  Recurse into bodies

               elsif Ekind (E) = E_Protected_Type
                       or else
                     Ekind (E) = E_Task_Type
                       or else
                     Ekind (E) = E_Subprogram_Body
                       or else
                     Ekind (E) = E_Package_Body
                       or else
                     Ekind (E) = E_Task_Body
                       or else
                     Ekind (E) = E_Protected_Body
               then
                  List_Entities (E);

               --  Recurse into blocks

               elsif Ekind (E) = E_Block then
                  List_Entities (E);
               end if;
            end if;

            E := Next_Entity (E);
         end loop;

         --  For a package body, the entities of the visible subprograms are
         --  declared in the corresponding spec. Iterate over its entities in
         --  order to handle properly the subprogram bodies. Skip bodies in
         --  subunits, which are listed independently.

         if Ekind (Ent) = E_Package_Body
           and then Present (Corresponding_Spec (Find_Declaration (Ent)))
         then
            E := First_Entity (Corresponding_Spec (Find_Declaration (Ent)));

            while Present (E) loop
               if Is_Subprogram (E)
                 and then
                   Nkind (Find_Declaration (E)) = N_Subprogram_Declaration
               then
                  Body_E := Corresponding_Body (Find_Declaration (E));

                  if Present (Body_E)
                    and then
                      Nkind (Parent (Find_Declaration (Body_E))) /= N_Subunit
                  then
                     List_Entities (Body_E);
                  end if;
               end if;

               Next_Entity (E);
            end loop;
         end if;
      end if;
   end List_Entities;

   -------------------------
   -- List_GCC_Expression --
   -------------------------

   procedure List_GCC_Expression (U : Node_Ref_Or_Val) is

      procedure Print_Expr (Val : Node_Ref_Or_Val);
      --  Internal recursive procedure to print expression

      ----------------
      -- Print_Expr --
      ----------------

      procedure Print_Expr (Val : Node_Ref_Or_Val) is
      begin
         if Val >= 0 then
            UI_Write (Val, Decimal);

         else
            declare
               Node : Exp_Node renames Rep_Table.Table (-UI_To_Int (Val));

               procedure Binop (S : String);
               --  Output text for binary operator with S being operator name

               -----------
               -- Binop --
               -----------

               procedure Binop (S : String) is
               begin
                  Write_Char ('(');
                  Print_Expr (Node.Op1);
                  Write_Str (S);
                  Print_Expr (Node.Op2);
                  Write_Char (')');
               end Binop;

            --  Start of processing for Print_Expr

            begin
               case Node.Expr is
                  when Cond_Expr =>
                     Write_Str ("(if ");
                     Print_Expr (Node.Op1);
                     Write_Str (" then ");
                     Print_Expr (Node.Op2);
                     Write_Str (" else ");
                     Print_Expr (Node.Op3);
                     Write_Str (" end)");

                  when Plus_Expr =>
                     Binop (" + ");

                  when Minus_Expr =>
                     Binop (" - ");

                  when Mult_Expr =>
                     Binop (" * ");

                  when Trunc_Div_Expr =>
                     Binop (" /t ");

                  when Ceil_Div_Expr =>
                     Binop (" /c ");

                  when Floor_Div_Expr =>
                     Binop (" /f ");

                  when Trunc_Mod_Expr =>
                     Binop (" modt ");

                  when Floor_Mod_Expr =>
                     Binop (" modf ");

                  when Ceil_Mod_Expr =>
                     Binop (" modc ");

                  when Exact_Div_Expr =>
                     Binop (" /e ");

                  when Negate_Expr =>
                     Write_Char ('-');
                     Print_Expr (Node.Op1);

                  when Min_Expr =>
                     Binop (" min ");

                  when Max_Expr =>
                     Binop (" max ");

                  when Abs_Expr =>
                     Write_Str ("abs ");
                     Print_Expr (Node.Op1);

                  when Truth_Andif_Expr =>
                     Binop (" and if ");

                  when Truth_Orif_Expr =>
                     Binop (" or if ");

                  when Truth_And_Expr =>
                     Binop (" and ");

                  when Truth_Or_Expr =>
                     Binop (" or ");

                  when Truth_Xor_Expr =>
                     Binop (" xor ");

                  when Truth_Not_Expr =>
                     Write_Str ("not ");
                     Print_Expr (Node.Op1);

                  when Bit_And_Expr =>
                     Binop (" & ");

                  when Lt_Expr =>
                     Binop (" < ");

                  when Le_Expr =>
                     Binop (" <= ");

                  when Gt_Expr =>
                     Binop (" > ");

                  when Ge_Expr =>
                     Binop (" >= ");

                  when Eq_Expr =>
                     Binop (" == ");

                  when Ne_Expr =>
                     Binop (" != ");

                  when Discrim_Val =>
                     Write_Char ('#');
                     UI_Write (Node.Op1);

               end case;
            end;
         end if;
      end Print_Expr;

   --  Start of processing for List_GCC_Expression

   begin
      if U = No_Uint then
         Write_Str ("??");
      else
         Print_Expr (U);
      end if;
   end List_GCC_Expression;

   ---------------------
   -- List_Mechanisms --
   ---------------------

   procedure List_Mechanisms (Ent : Entity_Id) is
      Plen : Natural;
      Form : Entity_Id;

   begin
      Blank_Line;

      case Ekind (Ent) is
         when E_Function =>
            Write_Str ("function ");

         when E_Operator =>
            Write_Str ("operator ");

         when E_Procedure =>
            Write_Str ("procedure ");

         when E_Subprogram_Type =>
            Write_Str ("type ");

         when E_Entry | E_Entry_Family =>
            Write_Str ("entry ");

         when others =>
            raise Program_Error;
      end case;

      Get_Unqualified_Decoded_Name_String (Chars (Ent));
      Write_Str (Name_Buffer (1 .. Name_Len));
      Write_Str (" declared at ");
      Write_Location (Sloc (Ent));
      Write_Eol;

      Write_Str ("  convention : ");

      case Convention (Ent) is
         when Convention_Ada       => Write_Line ("Ada");
         when Convention_Intrinsic => Write_Line ("InLineinsic");
         when Convention_Entry     => Write_Line ("Entry");
         when Convention_Protected => Write_Line ("Protected");
         when Convention_Assembler => Write_Line ("Assembler");
         when Convention_C         => Write_Line ("C");
         when Convention_CIL       => Write_Line ("CIL");
         when Convention_COBOL     => Write_Line ("COBOL");
         when Convention_CPP       => Write_Line ("C++");
         when Convention_Fortran   => Write_Line ("Fortran");
         when Convention_Java      => Write_Line ("Java");
         when Convention_Stdcall   => Write_Line ("Stdcall");
         when Convention_Stubbed   => Write_Line ("Stubbed");
      end case;

      --  Find max length of formal name

      Plen := 0;
      Form := First_Formal (Ent);
      while Present (Form) loop
         Get_Unqualified_Decoded_Name_String (Chars (Form));

         if Name_Len > Plen then
            Plen := Name_Len;
         end if;

         Next_Formal (Form);
      end loop;

      --  Output formals and mechanisms

      Form := First_Formal (Ent);
      while Present (Form) loop
         Get_Unqualified_Decoded_Name_String (Chars (Form));

         while Name_Len <= Plen loop
            Name_Len := Name_Len + 1;
            Name_Buffer (Name_Len) := ' ';
         end loop;

         Write_Str ("  ");
         Write_Str (Name_Buffer (1 .. Plen + 1));
         Write_Str (": passed by ");

         Write_Mechanism (Mechanism (Form));
         Write_Eol;
         Next_Formal (Form);
      end loop;

      if Etype (Ent) /= Standard_Void_Type then
         Write_Str ("  returns by ");
         Write_Mechanism (Mechanism (Ent));
         Write_Eol;
      end if;
   end List_Mechanisms;

   ---------------
   -- List_Name --
   ---------------

   procedure List_Name (Ent : Entity_Id) is
   begin
      if not Is_Compilation_Unit (Scope (Ent)) then
         List_Name (Scope (Ent));
         Write_Char ('.');
      end if;

      Get_Unqualified_Decoded_Name_String (Chars (Ent));
      Set_Casing (Unit_Casing);
      Write_Str (Name_Buffer (1 .. Name_Len));
   end List_Name;

   ---------------------
   -- List_Object_Info --
   ---------------------

   procedure List_Object_Info (Ent : Entity_Id) is
   begin
      Blank_Line;

      Write_Str ("for ");
      List_Name (Ent);
      Write_Str ("'Size use ");
      Write_Val (Esize (Ent));
      Write_Line (";");

      Write_Str ("for ");
      List_Name (Ent);
      Write_Str ("'Alignment use ");
      Write_Val (Alignment (Ent));
      Write_Line (";");
   end List_Object_Info;

   ----------------------
   -- List_Record_Info --
   ----------------------

   procedure List_Record_Info (Ent : Entity_Id) is
      Comp  : Entity_Id;
      Cfbit : Uint;
      Sunit : Uint;

      Max_Name_Length : Natural;
      Max_Suni_Length : Natural;

   begin
      Blank_Line;
      List_Type_Info (Ent);

      Write_Str ("for ");
      List_Name (Ent);
      Write_Line (" use record");

      --  First loop finds out max line length and max starting position
      --  length, for the purpose of lining things up nicely.

      Max_Name_Length := 0;
      Max_Suni_Length := 0;

      Comp := First_Component_Or_Discriminant (Ent);
      while Present (Comp) loop
         Get_Decoded_Name_String (Chars (Comp));
         Max_Name_Length := Natural'Max (Max_Name_Length, Name_Len);

         Cfbit := Component_Bit_Offset (Comp);

         if Rep_Not_Constant (Cfbit) then
            UI_Image_Length := 2;

         else
            --  Complete annotation in case not done

            Set_Normalized_Position (Comp, Cfbit / SSU);
            Set_Normalized_First_Bit (Comp, Cfbit mod SSU);

            Sunit := Cfbit / SSU;
            UI_Image (Sunit);
         end if;

         --  If the record is not packed, then we know that all fields whose
         --  position is not specified have a starting normalized bit position
         --  of zero.

         if Unknown_Normalized_First_Bit (Comp)
           and then not Is_Packed (Ent)
         then
            Set_Normalized_First_Bit (Comp, Uint_0);
         end if;

         Max_Suni_Length :=
           Natural'Max (Max_Suni_Length, UI_Image_Length);

         Next_Component_Or_Discriminant (Comp);
      end loop;

      --  Second loop does actual output based on those values

      Comp := First_Component_Or_Discriminant (Ent);
      while Present (Comp) loop
         declare
            Esiz : constant Uint := Esize (Comp);
            Bofs : constant Uint := Component_Bit_Offset (Comp);
            Npos : constant Uint := Normalized_Position (Comp);
            Fbit : constant Uint := Normalized_First_Bit (Comp);
            Lbit : Uint;

         begin
            Write_Str ("   ");
            Get_Decoded_Name_String (Chars (Comp));
            Set_Casing (Unit_Casing);
            Write_Str (Name_Buffer (1 .. Name_Len));

            for J in 1 .. Max_Name_Length - Name_Len loop
               Write_Char (' ');
            end loop;

            Write_Str (" at ");

            if Known_Static_Normalized_Position (Comp) then
               UI_Image (Npos);
               Spaces (Max_Suni_Length - UI_Image_Length);
               Write_Str (UI_Image_Buffer (1 .. UI_Image_Length));

            elsif Known_Component_Bit_Offset (Comp)
              and then List_Representation_Info = 3
            then
               Spaces (Max_Suni_Length - 2);
               Write_Str ("bit offset");
               Write_Val (Bofs, Paren => True);
               Write_Str (" size in bits = ");
               Write_Val (Esiz, Paren => True);
               Write_Eol;
               goto Continue;

            elsif Known_Normalized_Position (Comp)
              and then List_Representation_Info = 3
            then
               Spaces (Max_Suni_Length - 2);
               Write_Val (Npos);

            else
               --  For the packed case, we don't know the bit positions if we
               --  don't know the starting position!

               if Is_Packed (Ent) then
                  Write_Line ("?? range  ? .. ??;");
                  goto Continue;

               --  Otherwise we can continue

               else
                  Write_Str ("??");
               end if;
            end if;

            Write_Str (" range  ");
            UI_Write (Fbit);
            Write_Str (" .. ");

            --  Allowing Uint_0 here is a kludge, really this should be a
            --  fine Esize value but currently it means unknown, except that
            --  we know after gigi has back annotated that a size of zero is
            --  real, since otherwise gigi back annotates using No_Uint as
            --  the value to indicate unknown).

            if (Esize (Comp) = Uint_0 or else Known_Static_Esize (Comp))
              and then Known_Static_Normalized_First_Bit (Comp)
            then
               Lbit := Fbit + Esiz - 1;

               if Lbit < 10 then
                  Write_Char (' ');
               end if;

               UI_Write (Lbit);

            --  The test for Esize (Comp) not being Uint_0 here is a kludge.
            --  Officially a value of zero for Esize means unknown, but here
            --  we use the fact that we know that gigi annotates Esize with
            --  No_Uint, not Uint_0. Really everyone should use No_Uint???

            elsif List_Representation_Info < 3
              or else (Esize (Comp) /= Uint_0 and then Unknown_Esize (Comp))
            then
               Write_Str ("??");

            --  List_Representation >= 3 and Known_Esize (Comp)

            else
               Write_Val (Esiz, Paren => True);

               --  If in front end layout mode, then dynamic size is stored
               --  in storage units, so renormalize for output

               if not Back_End_Layout then
                  Write_Str (" * ");
                  Write_Int (SSU);
               end if;

               --  Add appropriate first bit offset

               if Fbit = 0 then
                  Write_Str (" - 1");

               elsif Fbit = 1 then
                  null;

               else
                  Write_Str (" + ");
                  Write_Int (UI_To_Int (Fbit) - 1);
               end if;
            end if;

            Write_Line (";");
         end;

      <<Continue>>
         Next_Component_Or_Discriminant (Comp);
      end loop;

      Write_Line ("end record;");
   end List_Record_Info;

   -------------------
   -- List_Rep_Info --
   -------------------

   procedure List_Rep_Info is
      Col : Nat;

   begin
      if List_Representation_Info /= 0
        or else List_Representation_Info_Mechanisms
      then
         for U in Main_Unit .. Last_Unit loop
            if In_Extended_Main_Source_Unit (Cunit_Entity (U)) then

               --  Normal case, list to standard output

               if not List_Representation_Info_To_File then
                  Unit_Casing := Identifier_Casing (Source_Index (U));
                  Write_Eol;
                  Write_Str ("Representation information for unit ");
                  Write_Unit_Name (Unit_Name (U));
                  Col := Column;
                  Write_Eol;

                  for J in 1 .. Col - 1 loop
                     Write_Char ('-');
                  end loop;

                  Write_Eol;
                  List_Entities (Cunit_Entity (U));

               --  List representation information to file

               else
                  Create_Repinfo_File_Access.all
                    (Get_Name_String (File_Name (Source_Index (U))));
                  Set_Special_Output (Write_Info_Line'Access);
                  List_Entities (Cunit_Entity (U));
                  Set_Special_Output (null);
                  Close_Repinfo_File_Access.all;
               end if;
            end if;
         end loop;
      end if;
   end List_Rep_Info;

   --------------------
   -- List_Type_Info --
   --------------------

   procedure List_Type_Info (Ent : Entity_Id) is
   begin
      Blank_Line;

      --  Do not list size info for unconstrained arrays, not meaningful

      if Is_Array_Type (Ent) and then not Is_Constrained (Ent) then
         null;

      else
         --  If Esize and RM_Size are the same and known, list as Size. This
         --  is a common case, which we may as well list in simple form.

         if Esize (Ent) = RM_Size (Ent) then
            Write_Str ("for ");
            List_Name (Ent);
            Write_Str ("'Size use ");
            Write_Val (Esize (Ent));
            Write_Line (";");

         --  For now, temporary case, to be removed when gigi properly back
         --  annotates RM_Size, if RM_Size is not set, then list Esize as Size.
         --  This avoids odd Object_Size output till we fix things???

         elsif Unknown_RM_Size (Ent) then
            Write_Str ("for ");
            List_Name (Ent);
            Write_Str ("'Size use ");
            Write_Val (Esize (Ent));
            Write_Line (";");

         --  Otherwise list size values separately if they are set

         else
            Write_Str ("for ");
            List_Name (Ent);
            Write_Str ("'Object_Size use ");
            Write_Val (Esize (Ent));
            Write_Line (";");

            --  Note on following check: The RM_Size of a discrete type can
            --  legitimately be set to zero, so a special check is needed.

            Write_Str ("for ");
            List_Name (Ent);
            Write_Str ("'Value_Size use ");
            Write_Val (RM_Size (Ent));
            Write_Line (";");
         end if;
      end if;

      Write_Str ("for ");
      List_Name (Ent);
      Write_Str ("'Alignment use ");
      Write_Val (Alignment (Ent));
      Write_Line (";");
   end List_Type_Info;

   ----------------------
   -- Rep_Not_Constant --
   ----------------------

   function Rep_Not_Constant (Val : Node_Ref_Or_Val) return Boolean is
   begin
      if Val = No_Uint or else Val < 0 then
         return True;
      else
         return False;
      end if;
   end Rep_Not_Constant;

   ---------------
   -- Rep_Value --
   ---------------

   function Rep_Value
     (Val : Node_Ref_Or_Val;
      D   : Discrim_List) return Uint
   is
      function B (Val : Boolean) return Uint;
      --  Returns Uint_0 for False, Uint_1 for True

      function T (Val : Node_Ref_Or_Val) return Boolean;
      --  Returns True for 0, False for any non-zero (i.e. True)

      function V (Val : Node_Ref_Or_Val) return Uint;
      --  Internal recursive routine to evaluate tree

      function W (Val : Uint) return Word;
      --  Convert Val to Word, assuming Val is always in the Int range. This is
      --  a helper function for the evaluation of bitwise expressions like
      --  Bit_And_Expr, for which there is no direct support in uintp. Uint
      --  values out of the Int range are expected to be seen in such
      --  expressions only with overflowing byte sizes around, introducing
      --  inherent unreliabilties in computations anyway.

      -------
      -- B --
      -------

      function B (Val : Boolean) return Uint is
      begin
         if Val then
            return Uint_1;
         else
            return Uint_0;
         end if;
      end B;

      -------
      -- T --
      -------

      function T (Val : Node_Ref_Or_Val) return Boolean is
      begin
         if V (Val) = 0 then
            return False;
         else
            return True;
         end if;
      end T;

      -------
      -- V --
      -------

      function V (Val : Node_Ref_Or_Val) return Uint is
         L, R, Q : Uint;

      begin
         if Val >= 0 then
            return Val;

         else
            declare
               Node : Exp_Node renames Rep_Table.Table (-UI_To_Int (Val));

            begin
               case Node.Expr is
                  when Cond_Expr =>
                     if T (Node.Op1) then
                        return V (Node.Op2);
                     else
                        return V (Node.Op3);
                     end if;

                  when Plus_Expr =>
                     return V (Node.Op1) + V (Node.Op2);

                  when Minus_Expr =>
                     return V (Node.Op1) - V (Node.Op2);

                  when Mult_Expr =>
                     return V (Node.Op1) * V (Node.Op2);

                  when Trunc_Div_Expr =>
                     return V (Node.Op1) / V (Node.Op2);

                  when Ceil_Div_Expr =>
                     return
                       UR_Ceiling
                         (V (Node.Op1) / UR_From_Uint (V (Node.Op2)));

                  when Floor_Div_Expr =>
                     return
                       UR_Floor
                         (V (Node.Op1) / UR_From_Uint (V (Node.Op2)));

                  when Trunc_Mod_Expr =>
                     return V (Node.Op1) rem V (Node.Op2);

                  when Floor_Mod_Expr =>
                     return V (Node.Op1) mod V (Node.Op2);

                  when Ceil_Mod_Expr =>
                     L := V (Node.Op1);
                     R := V (Node.Op2);
                     Q := UR_Ceiling (L / UR_From_Uint (R));
                     return L - R * Q;

                  when Exact_Div_Expr =>
                     return V (Node.Op1) / V (Node.Op2);

                  when Negate_Expr =>
                     return -V (Node.Op1);

                  when Min_Expr =>
                     return UI_Min (V (Node.Op1), V (Node.Op2));

                  when Max_Expr =>
                     return UI_Max (V (Node.Op1), V (Node.Op2));

                  when Abs_Expr =>
                     return UI_Abs (V (Node.Op1));

                  when Truth_Andif_Expr =>
                     return B (T (Node.Op1) and then T (Node.Op2));

                  when Truth_Orif_Expr =>
                     return B (T (Node.Op1) or else T (Node.Op2));

                  when Truth_And_Expr =>
                     return B (T (Node.Op1) and T (Node.Op2));

                  when Truth_Or_Expr =>
                     return B (T (Node.Op1) or T (Node.Op2));

                  when Truth_Xor_Expr =>
                     return B (T (Node.Op1) xor T (Node.Op2));

                  when Truth_Not_Expr =>
                     return B (not T (Node.Op1));

                  when Bit_And_Expr =>
                     L := V (Node.Op1);
                     R := V (Node.Op2);
                     return UI_From_Int (Int (W (L) and W (R)));

                  when Lt_Expr =>
                     return B (V (Node.Op1) < V (Node.Op2));

                  when Le_Expr =>
                     return B (V (Node.Op1) <= V (Node.Op2));

                  when Gt_Expr =>
                     return B (V (Node.Op1) > V (Node.Op2));

                  when Ge_Expr =>
                     return B (V (Node.Op1) >= V (Node.Op2));

                  when Eq_Expr =>
                     return B (V (Node.Op1) = V (Node.Op2));

                  when Ne_Expr =>
                     return B (V (Node.Op1) /= V (Node.Op2));

                  when Discrim_Val =>
                     declare
                        Sub : constant Int := UI_To_Int (Node.Op1);

                     begin
                        pragma Assert (Sub in D'Range);
                        return D (Sub);
                     end;

               end case;
            end;
         end if;
      end V;

      -------
      -- W --
      -------

      --  We use an unchecked conversion to map Int values to their Word
      --  bitwise equivalent, which we could not achieve with a normal type
      --  conversion for negative Ints. We want bitwise equivalents because W
      --  is used as a helper for bit operators like Bit_And_Expr, and can be
      --  called for negative Ints in the context of aligning expressions like
      --  X+Align & -Align.

      function W (Val : Uint) return Word is
         function To_Word is new Ada.Unchecked_Conversion (Int, Word);
      begin
         return To_Word (UI_To_Int (Val));
      end W;

   --  Start of processing for Rep_Value

   begin
      if Val = No_Uint then
         return No_Uint;

      else
         return V (Val);
      end if;
   end Rep_Value;

   ------------
   -- Spaces --
   ------------

   procedure Spaces (N : Natural) is
   begin
      for J in 1 .. N loop
         Write_Char (' ');
      end loop;
   end Spaces;

   ---------------
   -- Tree_Read --
   ---------------

   procedure Tree_Read is
   begin
      Rep_Table.Tree_Read;
   end Tree_Read;

   ----------------
   -- Tree_Write --
   ----------------

   procedure Tree_Write is
   begin
      Rep_Table.Tree_Write;
   end Tree_Write;

   ---------------------
   -- Write_Info_Line --
   ---------------------

   procedure Write_Info_Line (S : String) is
   begin
      Write_Repinfo_Line_Access.all (S (S'First .. S'Last - 1));
   end Write_Info_Line;

   ---------------------
   -- Write_Mechanism --
   ---------------------

   procedure Write_Mechanism (M : Mechanism_Type) is
   begin
      case M is
         when 0 =>
            Write_Str ("default");

         when -1 =>
            Write_Str ("copy");

         when -2 =>
            Write_Str ("reference");

         when -3 =>
            Write_Str ("descriptor");

         when -4 =>
            Write_Str ("descriptor (UBS)");

         when -5 =>
            Write_Str ("descriptor (UBSB)");

         when -6 =>
            Write_Str ("descriptor (UBA)");

         when -7 =>
            Write_Str ("descriptor (S)");

         when -8 =>
            Write_Str ("descriptor (SB)");

         when -9 =>
            Write_Str ("descriptor (A)");

         when -10 =>
            Write_Str ("descriptor (NCA)");

         when others =>
            raise Program_Error;
      end case;
   end Write_Mechanism;

   ---------------
   -- Write_Val --
   ---------------

   procedure Write_Val (Val : Node_Ref_Or_Val; Paren : Boolean := False) is
   begin
      if Rep_Not_Constant (Val) then
         if List_Representation_Info < 3 or else Val = No_Uint then
            Write_Str ("??");

         else
            if Back_End_Layout then
               Write_Char (' ');

               if Paren then
                  Write_Char ('(');
                  List_GCC_Expression (Val);
                  Write_Char (')');
               else
                  List_GCC_Expression (Val);
               end if;

               Write_Char (' ');

            else
               if Paren then
                  Write_Char ('(');
                  Write_Name_Decoded (Chars (Get_Dynamic_SO_Entity (Val)));
                  Write_Char (')');
               else
                  Write_Name_Decoded (Chars (Get_Dynamic_SO_Entity (Val)));
               end if;
            end if;
         end if;

      else
         UI_Write (Val);
      end if;
   end Write_Val;

end Repinfo;
