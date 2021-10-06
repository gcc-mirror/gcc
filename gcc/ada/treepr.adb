------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               T R E E P R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

with Aspects;        use Aspects;
with Atree;          use Atree;
with Csets;          use Csets;
with Debug;          use Debug;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Elists;         use Elists;
with Lib;            use Lib;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Output;         use Output;
with Seinfo;         use Seinfo;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Snames;         use Snames;
with Sinput;         use Sinput;
with Stand;          use Stand;
with Stringt;        use Stringt;
with SCIL_LL;        use SCIL_LL;
with Uintp;          use Uintp;
with Urealp;         use Urealp;
with Uname;          use Uname;
with Unchecked_Conversion;
with Unchecked_Deallocation;

package body Treepr is

   ----------------------------------
   -- Approach Used for Tree Print --
   ----------------------------------

   --  When a complete subtree is being printed, a trace phase first marks
   --  the nodes and lists to be printed. This trace phase allocates logical
   --  numbers corresponding to the order in which the nodes and lists will
   --  be printed. The Node_Id, List_Id and Elist_Id values are mapped to
   --  logical node numbers using a hash table. Output is done using a set
   --  of Print_xxx routines, which are similar to the Write_xxx routines
   --  with the same name, except that they do not generate any output in
   --  the marking phase. This allows identical logic to be used in the
   --  two phases.

   --  Note that the hash table not only holds the serial numbers, but also
   --  acts as a record of which nodes have already been visited. In the
   --  marking phase, a node has been visited if it is already in the hash
   --  table, and in the printing phase, we can tell whether a node has
   --  already been printed by looking at the value of the serial number.

   ----------------------
   -- Global Variables --
   ----------------------

   Print_Low_Level_Info : Boolean := False with Warnings => Off;
   --  Set True to print low-level information useful for debugging Atree and
   --  the like.

   type Hash_Record is record
      Serial : Nat;
      --  Serial number for hash table entry. A value of zero means that
      --  the entry is currently unused.

      Id : Int;
      --  If serial number field is non-zero, contains corresponding Id value
   end record;

   type Hash_Table_Type is array (Nat range <>) of Hash_Record;
   type Access_Hash_Table_Type is access Hash_Table_Type;
   Hash_Table : Access_Hash_Table_Type;
   --  The hash table itself, see Serial_Number function for details of use

   Hash_Table_Len : Nat;
   --  Range of Hash_Table is from 0 .. Hash_Table_Len - 1 so that dividing
   --  by Hash_Table_Len gives a remainder that is in Hash_Table'Range.

   Next_Serial_Number : Nat;
   --  Number of last visited node or list. Used during the marking phase to
   --  set proper node numbers in the hash table, and during the printing
   --  phase to make sure that a given node is not printed more than once.
   --  (nodes are printed in order during the printing phase, that's the
   --  point of numbering them in the first place).

   Printing_Descendants : Boolean;
   --  True if descendants are being printed, False if not. In the false case,
   --  only node Id's are printed. In the true case, node numbers as well as
   --  node Id's are printed, as described above.

   type Phase_Type is (Marking, Printing);
   --  Type for Phase variable

   Phase : Phase_Type;
   --  When an entire tree is being printed, the traversal operates in two
   --  phases. The first phase marks the nodes in use by installing node
   --  numbers in the node number table. The second phase prints the nodes.
   --  This variable indicates the current phase.

   ----------------------
   -- Local Procedures --
   ----------------------

   function From_Union is new Unchecked_Conversion (Union_Id, Uint);
   function From_Union is new Unchecked_Conversion (Union_Id, Ureal);

   function Capitalize (S : String) return String;
   procedure Capitalize (S : in out String);
   --  Turns an identifier into Mixed_Case

   function Image (F : Node_Or_Entity_Field) return String;

   procedure Print_Init;
   --  Initialize for printing of tree with descendants

   procedure Print_End_Span (N : Node_Id);
   --  Print contents of End_Span field of node N. The format includes the
   --  implicit source location as well as the value of the field.

   procedure Print_Term;
   --  Clean up after printing of tree with descendants

   procedure Print_Char (C : Character);
   --  Print character C if currently in print phase, noop if in marking phase

   procedure Print_Name (N : Name_Id);
   --  Print name from names table if currently in print phase, noop if in
   --  marking phase. Note that the name is output in mixed case mode.

   procedure Print_Node_Header (N : Node_Id);
   --  Print header line used by Print_Node and Print_Node_Briefly

   procedure Print_Node_Kind (N : Node_Id);
   --  Print node kind name in mixed case if in print phase, noop if in
   --  marking phase.

   procedure Print_Str (S : String);
   --  Print string S if currently in print phase, noop if in marking phase

   procedure Print_Str_Mixed_Case (S : String);
   --  Like Print_Str, except that the string is printed in mixed case mode

   procedure Print_Int (I : Int);
   --  Print integer I if currently in print phase, noop if in marking phase

   procedure Print_Eol;
   --  Print end of line if currently in print phase, noop if in marking phase

   procedure Print_Node_Ref (N : Node_Id);
   --  Print "<empty>", "<error>" or "Node #nnn" with additional information
   --  in the latter case, including the Id and the Nkind of the node.

   procedure Print_List_Ref (L : List_Id);
   --  Print "<no list>", or "<empty node list>" or "Node list #nnn"

   procedure Print_Elist_Ref (E : Elist_Id);
   --  Print "<no elist>", or "<empty element list>" or "Element list #nnn"

   procedure Print_Entity_Info (Ent : Entity_Id; Prefix : String);
   --  Called if the node being printed is an entity. Prints fields from the
   --  extension, using routines in Einfo to get the field names and flags.

   procedure Print_Field (Val : Union_Id; Format : UI_Format := Auto);
   procedure Print_Field
     (Prefix : String;
      Field  : String;
      N      : Node_Or_Entity_Id;
      FD     : Field_Descriptor;
      Format : UI_Format);
   --  Print representation of Field value (name, tree, string, uint, charcode)
   --  The format parameter controls the format of printing in the case of an
   --  integer value (see UI_Write for details).

   procedure Print_Node_Field
     (Prefix : String;
      Field  : Node_Field;
      N      : Node_Id;
      FD     : Field_Descriptor;
      Format : UI_Format := Auto);

   procedure Print_Entity_Field
     (Prefix : String;
      Field  : Entity_Field;
      N      : Entity_Id;
      FD     : Field_Descriptor;
      Format : UI_Format := Auto);

   procedure Print_Flag (F : Boolean);
   --  Print True or False

   procedure Print_Node
     (N           : Node_Id;
      Prefix_Str  : String;
      Prefix_Char : Character);
   --  This is the internal routine used to print a single node. Each line of
   --  output is preceded by Prefix_Str (which is used to set the indentation
   --  level and the bars used to link list elements). In addition, for lines
   --  other than the first, an additional character Prefix_Char is output.

   function Serial_Number (Id : Int) return Nat;
   --  Given a Node_Id, List_Id or Elist_Id, returns the previously assigned
   --  serial number, or zero if no serial number has yet been assigned.

   procedure Set_Serial_Number;
   --  Can be called only immediately following a call to Serial_Number that
   --  returned a value of zero. Causes the value of Next_Serial_Number to be
   --  placed in the hash table (corresponding to the Id argument used in the
   --  Serial_Number call), and increments Next_Serial_Number.

   procedure Visit_Node
     (N           : Node_Id;
      Prefix_Str  : String;
      Prefix_Char : Character);
   --  Called to process a single node in the case where descendants are to
   --  be printed before every line, and Prefix_Char added to all lines
   --  except the header line for the node.

   procedure Visit_List (L : List_Id; Prefix_Str : String);
   --  Visit_List is called to process a list in the case where descendants
   --  are to be printed. Prefix_Str is to be added to all printed lines.

   procedure Visit_Elist (E : Elist_Id; Prefix_Str : String);
   --  Visit_Elist is called to process an element list in the case where
   --  descendants are to be printed. Prefix_Str is to be added to all
   --  printed lines.

   ----------------
   -- Capitalize --
   ----------------

   procedure Capitalize (S : in out String) is
      Cap : Boolean := True;
   begin
      for J in S'Range loop
         declare
            Old : constant Character := S (J);
         begin
            if Cap then
               S (J) := Fold_Upper (S (J));
            else
               S (J) := Fold_Lower (S (J));
            end if;

            Cap := Old = '_';
         end;
      end loop;
   end Capitalize;

   function Capitalize (S : String) return String is
   begin
      return Result : String (S'Range) := S do
         Capitalize (Result);
      end return;
   end Capitalize;

   -----------
   -- Image --
   -----------

   function Image (F : Node_Or_Entity_Field) return String is
   begin
      case F is
         when F_Alloc_For_BIP_Return =>
            return "Alloc_For_BIP_Return";
         when F_Assignment_OK =>
            return "Assignment_OK";
         when F_Backwards_OK =>
            return "Backwards_OK";
         when F_Conversion_OK =>
            return "Conversion_OK";
         when F_Forwards_OK =>
            return "Forwards_OK";
         when F_Has_SP_Choice =>
            return "Has_SP_Choice";
         when F_Is_Elaboration_Checks_OK_Node =>
            return "Is_Elaboration_Checks_OK_Node";
         when F_Is_Elaboration_Warnings_OK_Node =>
            return "Is_Elaboration_Warnings_OK_Node";
         when F_Is_Known_Guaranteed_ABE =>
            return "Is_Known_Guaranteed_ABE";
         when F_Is_SPARK_Mode_On_Node =>
            return "Is_SPARK_Mode_On_Node";
         when F_Local_Raise_Not_OK =>
            return "Local_Raise_Not_OK";
         when F_SCIL_Controlling_Tag =>
            return "SCIL_Controlling_Tag";
         when F_SCIL_Entity =>
            return "SCIL_Entity";
         when F_SCIL_Tag_Value =>
            return "SCIL_Tag_Value";
         when F_SCIL_Target_Prim =>
            return "SCIL_Target_Prim";
         when F_Shift_Count_OK =>
            return "Shift_Count_OK";
         when F_Split_PPC =>
            return "Split_PPC";
         when F_TSS_Elist =>
            return "TSS_Elist";

         when F_BIP_Initialization_Call =>
            return "BIP_Initialization_Call";
         when F_Body_Needed_For_SAL =>
            return "Body_Needed_For_SAL";
         when F_CR_Discriminant =>
            return "CR_Discriminant";
         when F_DT_Entry_Count =>
            return "DT_Entry_Count";
         when F_DT_Offset_To_Top_Func =>
            return "DT_Offset_To_Top_Func";
         when F_DT_Position =>
            return "DT_Position";
         when F_DTC_Entity =>
            return "DTC_Entity";
         when F_Has_Inherited_DIC =>
            return "Has_Inherited_DIC";
         when F_Has_Own_DIC =>
            return "Has_Own_DIC";
         when F_Has_RACW =>
            return "Has_RACW";
         when F_Ignore_SPARK_Mode_Pragmas =>
            return "Ignore_SPARK_Mode_Pragmas";
         when F_Is_Constr_Subt_For_UN_Aliased =>
            return "Is_Constr_Subt_For_UN_Aliased";
         when F_Is_CPP_Class =>
            return "Is_CPP_Class";
         when F_Is_CUDA_Kernel =>
            return "Is_CUDA_Kernel";
         when F_Is_DIC_Procedure =>
            return "Is_DIC_Procedure";
         when F_Is_Discrim_SO_Function =>
            return "Is_Discrim_SO_Function";
         when F_Is_Elaboration_Checks_OK_Id =>
            return "Is_Elaboration_Checks_OK_Id";
         when F_Is_Elaboration_Warnings_OK_Id =>
            return "Is_Elaboration_Warnings_OK_Id";
         when F_Is_RACW_Stub_Type =>
            return "Is_RACW_Stub_Type";
         when F_LSP_Subprogram =>
            return "LSP_Subprogram";
         when F_OK_To_Rename =>
            return "OK_To_Rename";
         when F_Referenced_As_LHS =>
            return "Referenced_As_LHS";
         when F_RM_Size =>
            return "RM_Size";
         when F_SPARK_Aux_Pragma =>
            return "SPARK_Aux_Pragma";
         when F_SPARK_Aux_Pragma_Inherited =>
            return "SPARK_Aux_Pragma_Inherited";
         when F_SPARK_Pragma =>
            return "SPARK_Pragma";
         when F_SPARK_Pragma_Inherited =>
            return "SPARK_Pragma_Inherited";
         when F_SSO_Set_High_By_Default =>
            return "SSO_Set_High_By_Default";
         when F_SSO_Set_Low_By_Default =>
            return "SSO_Set_Low_By_Default";

         when others =>
            declare
               Result : constant String := Capitalize (F'Img);
            begin
               return Result (3 .. Result'Last); -- Remove "F_"
            end;
      end case;
   end Image;

   -------
   -- p --
   -------

   function p (N : Union_Id) return Node_Or_Entity_Id is
   begin
      case N is
         when List_Low_Bound .. List_High_Bound - 1 =>
            return Nlists.Parent (List_Id (N));

         when Node_Range =>
            return Parent (Node_Or_Entity_Id (N));

         when others =>
            Write_Int (Int (N));
            Write_Str (" is not a Node_Id or List_Id value");
            Write_Eol;
            return Empty;
      end case;
   end p;

   ---------
   -- par --
   ---------

   function par (N : Union_Id) return Node_Or_Entity_Id renames p;

   procedure ppar (N : Union_Id) is
   begin
      if N /= Empty_List_Or_Node then
         pp (N);
         ppar (Union_Id (p (N)));
      end if;
   end ppar;

   --------
   -- pe --
   --------

   procedure pe (N : Union_Id) renames pn;

   --------
   -- pl --
   --------

   procedure pl (L : Int) is
      Lid : Int;

   begin
      Push_Output;
      Set_Standard_Output;

      if L < 0 then
         Lid := L;

      --  This is the case where we transform e.g. +36 to -99999936

      else
         if L <= 9 then
            Lid := -(99999990 + L);
         elsif L <= 99 then
            Lid := -(99999900 + L);
         elsif L <= 999 then
            Lid := -(99999000 + L);
         elsif L <= 9999 then
            Lid := -(99990000 + L);
         elsif L <= 99999 then
            Lid := -(99900000 + L);
         elsif L <= 999999 then
            Lid := -(99000000 + L);
         elsif L <= 9999999 then
            Lid := -(90000000 + L);
         else
            Lid := -L;
         end if;
      end if;

      --  Now output the list

      Print_Tree_List (List_Id (Lid));
      Pop_Output;
   end pl;

   --------
   -- pn --
   --------

   procedure pn (N : Union_Id) is
   begin
      Push_Output;
      Set_Standard_Output;

      case N is
         when List_Low_Bound .. List_High_Bound - 1 =>
            pl (Int (N));
         when Node_Range =>
            Print_Tree_Node (Node_Id (N));
         when Elist_Range =>
            Print_Tree_Elist (Elist_Id (N));
         when Elmt_Range =>
            declare
               Id : constant Elmt_Id := Elmt_Id (N);
            begin
               if No (Id) then
                  Write_Str ("No_Elmt");
                  Write_Eol;
               else
                  Write_Str ("Elmt_Id --> ");
                  Print_Tree_Node (Node (Id));
               end if;
            end;
         when Names_Range =>
            Namet.wn (Name_Id (N));
         when Strings_Range =>
            Write_String_Table_Entry (String_Id (N));
         when Uint_Range =>
            Uintp.pid (From_Union (N));
         when Ureal_Range =>
            Urealp.pr (From_Union (N));
         when others =>
            Write_Str ("Invalid Union_Id: ");
            Write_Int (Int (N));
            Write_Eol;
      end case;

      Pop_Output;
   end pn;

   --------
   -- pp --
   --------

   procedure pp (N : Union_Id) renames pn;

   ---------
   -- ppp --
   ---------

   procedure ppp (N : Union_Id) renames pt;

   ----------------
   -- Print_Char --
   ----------------

   procedure Print_Char (C : Character) is
   begin
      if Phase = Printing then
         Write_Char (C);
      end if;
   end Print_Char;

   ---------------------
   -- Print_Elist_Ref --
   ---------------------

   procedure Print_Elist_Ref (E : Elist_Id) is
   begin
      if Phase /= Printing then
         return;
      end if;

      if E = No_Elist then
         Write_Str ("<no elist>");

      elsif Is_Empty_Elmt_List (E) then
         Write_Str ("Empty elist, (Elist_Id=");
         Write_Int (Int (E));
         Write_Char (')');

      else
         Write_Str ("(Elist_Id=");
         Write_Int (Int (E));
         Write_Char (')');

         if Printing_Descendants then
            Write_Str (" #");
            Write_Int (Serial_Number (Int (E)));
         end if;
      end if;
   end Print_Elist_Ref;

   -------------------------
   -- Print_Elist_Subtree --
   -------------------------

   procedure Print_Elist_Subtree (E : Elist_Id) is
   begin
      Print_Init;

      Next_Serial_Number := 1;
      Phase := Marking;
      Visit_Elist (E, "");

      Next_Serial_Number := 1;
      Phase := Printing;
      Visit_Elist (E, "");

      Print_Term;
   end Print_Elist_Subtree;

   --------------------
   -- Print_End_Span --
   --------------------

   procedure Print_End_Span (N : Node_Id) is
      Val : constant Uint := End_Span (N);

   begin
      UI_Write (Val);
      Write_Str (" (Uint = ");
      Write_Str (UI_Image (Val));
      Write_Str (")  ");

      if Present (Val) then
         Write_Location (End_Location (N));
      end if;
   end Print_End_Span;

   -----------------------
   -- Print_Entity_Info --
   -----------------------

   procedure Print_Entity_Info (Ent : Entity_Id; Prefix : String) is
   begin
      Print_Str (Prefix);
      Print_Str ("Ekind = ");
      Print_Str_Mixed_Case (Entity_Kind'Image (Ekind (Ent)));
      Print_Eol;

      Print_Str (Prefix);
      Print_Str ("Etype = ");
      Print_Node_Ref (Etype (Ent));
      Print_Eol;

      if Convention (Ent) /= Convention_Ada then
         Print_Str (Prefix);
         Print_Str ("Convention = ");

         --  Print convention name skipping the Convention_ at the start

         declare
            S : constant String := Convention_Id'Image (Convention (Ent));

         begin
            Print_Str_Mixed_Case (S (12 .. S'Last));
            Print_Eol;
         end;
      end if;

      declare
         Fields : Entity_Field_Array renames
           Entity_Field_Table (Ekind (Ent)).all;
         Should_Print : constant Entity_Field_Set :=
           --  Set of fields that should be printed. False for fields that were
           --  already printed above.
           (F_Ekind
            | F_Basic_Convention => False, -- Convention was printed
            others => True);
      begin
         --  Outer loop makes flags come out last

         for Print_Flags in Boolean loop
            for Field_Index in Fields'Range loop
               declare
                  FD : Field_Descriptor renames
                    Field_Descriptors (Fields (Field_Index));
               begin
                  if Should_Print (Fields (Field_Index))
                    and then (FD.Kind = Flag_Field) = Print_Flags
                  then
                     Print_Entity_Field
                       (Prefix, Fields (Field_Index), Ent, FD);
                  end if;
               end;
            end loop;
         end loop;
      end;
   end Print_Entity_Info;

   ---------------
   -- Print_Eol --
   ---------------

   procedure Print_Eol is
   begin
      if Phase = Printing then
         Write_Eol;
      end if;
   end Print_Eol;

   -----------------
   -- Print_Field --
   -----------------

   --  Instantiations of low-level getters and setters that take offsets
   --  in units of the size of the field.

   use Atree.Atree_Private_Part;

   function Get_Flag is new Get_1_Bit_Field
     (Boolean) with Inline;

   function Get_Node_Id is new Get_32_Bit_Field
     (Node_Id) with Inline;

   function Get_List_Id is new Get_32_Bit_Field
     (List_Id) with Inline;

   function Get_Elist_Id is new Get_32_Bit_Field_With_Default
     (Elist_Id, No_Elist) with Inline;

   function Get_Name_Id is new Get_32_Bit_Field
     (Name_Id) with Inline;

   function Get_String_Id is new Get_32_Bit_Field
     (String_Id) with Inline;

   function Get_Uint is new Get_32_Bit_Field_With_Default
     (Uint, Uint_0) with Inline;

   function Get_Valid_Uint is new Get_32_Bit_Field
     (Uint) with Inline;
   --  Used for both Valid_Uint and other subtypes of Uint. Note that we don't
   --  instantiate Get_Valid_32_Bit_Field; we don't want to blow up if the
   --  value is wrong.

   function Get_Ureal is new Get_32_Bit_Field
     (Ureal) with Inline;

   function Get_Node_Kind_Type is new Get_8_Bit_Field
     (Node_Kind) with Inline;

   function Get_Entity_Kind_Type is new Get_8_Bit_Field
     (Entity_Kind) with Inline;

   function Get_Source_Ptr is new Get_32_Bit_Field
     (Source_Ptr) with Inline, Unreferenced;

   function Get_Small_Paren_Count_Type is new Get_2_Bit_Field
     (Small_Paren_Count_Type) with Inline, Unreferenced;

   function Get_Union_Id is new Get_32_Bit_Field
     (Union_Id) with Inline;

   function Get_Convention_Id is new Get_8_Bit_Field
     (Convention_Id) with Inline, Unreferenced;

   function Get_Mechanism_Type is new Get_32_Bit_Field
     (Mechanism_Type) with Inline, Unreferenced;

   procedure Print_Field (Val : Union_Id; Format : UI_Format := Auto) is
   begin
      if Phase /= Printing then
         return;
      end if;

      if Val in Node_Range then
         Print_Node_Ref (Node_Id (Val));

      elsif Val in List_Range then
         Print_List_Ref (List_Id (Val));

      elsif Val in Elist_Range then
         Print_Elist_Ref (Elist_Id (Val));

      elsif Val in Names_Range then
         Print_Name (Name_Id (Val));
         Write_Str (" (Name_Id=");
         Write_Int (Int (Val));
         Write_Char (')');

      elsif Val in Strings_Range then
         Write_String_Table_Entry (String_Id (Val));
         Write_Str (" (String_Id=");
         Write_Int (Int (Val));
         Write_Char (')');

      elsif Val in Uint_Range then
         UI_Write (From_Union (Val), Format);
         Write_Str (" (Uint = ");
         Write_Int (Int (Val));
         Write_Char (')');

      elsif Val in Ureal_Range then
         UR_Write (From_Union (Val));
         Write_Str (" (Ureal = ");
         Write_Int (Int (Val));
         Write_Char (')');

      else
         Print_Str ("****** Incorrect value = ");
         Print_Int (Int (Val));
      end if;
   end Print_Field;

   procedure Print_Field
     (Prefix : String;
      Field  : String;
      N      : Node_Or_Entity_Id;
      FD     : Field_Descriptor;
      Format : UI_Format)
   is
      Printed : Boolean := False;

      procedure Print_Initial;
      --  Print the initial stuff that goes before the value

      procedure Print_Initial is
      begin
         Printed := True;
         Print_Str (Prefix);
         Print_Str (Field);

         if Print_Low_Level_Info then
            Write_Str (" at ");
            Write_Int (Int (FD.Offset));
         end if;

         Write_Str (" = ");
      end Print_Initial;

   begin
      if Phase /= Printing then
         return;
      end if;

      case FD.Kind is
         when Flag_Field =>
            declare
               Val : constant Boolean := Get_Flag (N, FD.Offset);
            begin
               if Val then
                  Print_Initial;
                  Print_Flag (Val);
               end if;
            end;

         when Node_Id_Field =>
            declare
               Val : constant Node_Id := Get_Node_Id (N, FD.Offset);
            begin
               if Present (Val) then
                  Print_Initial;
                  Print_Node_Ref (Val);
               end if;
            end;

         when List_Id_Field =>
            declare
               Val : constant List_Id := Get_List_Id (N, FD.Offset);
            begin
               if Present (Val) then
                  Print_Initial;
                  Print_List_Ref (Val);
               end if;
            end;

         when Elist_Id_Field =>
            declare
               Val : constant Elist_Id := Get_Elist_Id (N, FD.Offset);
            begin
               if Present (Val) then
                  Print_Initial;
                  Print_Elist_Ref (Val);
               end if;
            end;

         when Name_Id_Field =>
            declare
               Val : constant Name_Id := Get_Name_Id (N, FD.Offset);
            begin
               if Present (Val) then
                  Print_Initial;
                  Print_Name (Val);
                  Write_Str (" (Name_Id=");
                  Write_Int (Int (Val));
                  Write_Char (')');
               end if;
            end;

         when String_Id_Field =>
            declare
               Val : constant String_Id := Get_String_Id (N, FD.Offset);
            begin
               if Val /= No_String then
                  Print_Initial;
                  Write_String_Table_Entry (Val);
                  Write_Str (" (String_Id=");
                  Write_Int (Int (Val));
                  Write_Char (')');
               end if;
            end;

         when Uint_Field =>
            declare
               Val : constant Uint := Get_Uint (N, FD.Offset);
               function Cast is new Unchecked_Conversion (Uint, Int);
            begin
               if Present (Val) then
                  Print_Initial;
                  UI_Write (Val, Format);
                  Write_Str (" (Uint = ");
                  Write_Int (Cast (Val));
                  Write_Char (')');
               end if;
            end;

         when Valid_Uint_Field | Unat_Field | Upos_Field
            | Nonzero_Uint_Field =>
            declare
               Val : constant Uint := Get_Valid_Uint (N, FD.Offset);
               function Cast is new Unchecked_Conversion (Uint, Int);
            begin
               Print_Initial;
               UI_Write (Val, Format);

               case FD.Kind is
                  when Valid_Uint_Field => Write_Str (" v");
                  when Unat_Field => Write_Str (" n");
                  when Upos_Field => Write_Str (" p");
                  when Nonzero_Uint_Field => Write_Str (" nz");
                  when others => raise Program_Error;
               end case;

               Write_Str (" (Uint = ");
               Write_Int (Cast (Val));
               Write_Char (')');
            end;

         when Ureal_Field =>
            declare
               Val : constant Ureal := Get_Ureal (N, FD.Offset);
               function Cast is new Unchecked_Conversion (Ureal, Int);
            begin
               if Val /= No_Ureal then
                  Print_Initial;
                  UR_Write (Val);
                  Write_Str (" (Ureal = ");
                  Write_Int (Cast (Val));
                  Write_Char (')');
               end if;
            end;

         when Node_Kind_Type_Field =>
            declare
               Val : constant Node_Kind := Get_Node_Kind_Type (N, FD.Offset);
            begin
               Print_Initial;
               Print_Str_Mixed_Case (Node_Kind'Image (Val));
            end;

         when Entity_Kind_Type_Field =>
            declare
               Val : constant Entity_Kind :=
                 Get_Entity_Kind_Type (N, FD.Offset);
            begin
               Print_Initial;
               Print_Str_Mixed_Case (Entity_Kind'Image (Val));
            end;

         when Union_Id_Field =>
            declare
               Val : constant Union_Id := Get_Union_Id (N, FD.Offset);
            begin
               if Val /= Empty_List_Or_Node then
                  Print_Initial;

                  if Val in Node_Range then
                     Print_Node_Ref (Node_Id (Val));

                  elsif Val in List_Range then
                     Print_List_Ref (List_Id (Val));

                  else
                     Print_Str ("<invalid union id>");
                  end if;
               end if;
            end;

         when others =>
            Print_Initial;
            Print_Str ("<unknown ");
            Print_Str (Field_Kind'Image (FD.Kind));
            Print_Str (">");
      end case;

      if Printed then
         Print_Eol;
      end if;

   --  If an exception is raised while printing, we try to print some low-level
   --  information that is useful for debugging.

   exception
      when others =>
         declare
            function Cast is new Unchecked_Conversion (Field_Size_32_Bit, Int);
         begin
            Write_Eol;
            Print_Initial;
            Write_Str ("exception raised in Print_Field -- int val = ");
            Write_Eol;

            case Field_Size (FD.Kind) is
               when 1 => Write_Int (Int (Get_1_Bit_Val (N, FD.Offset)));
               when 2 => Write_Int (Int (Get_2_Bit_Val (N, FD.Offset)));
               when 4 => Write_Int (Int (Get_4_Bit_Val (N, FD.Offset)));
               when 8 => Write_Int (Int (Get_8_Bit_Val (N, FD.Offset)));
               when others =>  -- 32
                  Write_Int (Cast (Get_32_Bit_Val (N, FD.Offset)));
            end case;

            Write_Str (", ");
            Write_Str (FD.Kind'Img);
            Write_Str (" ");
            Write_Int (Int (Field_Size (FD.Kind)));
            Write_Str (" bits");
            Write_Eol;
         exception
            when others =>
               Write_Eol;
               Write_Str ("double exception raised in Print_Field");
               Write_Eol;
         end;
   end Print_Field;

   ----------------------
   -- Print_Node_Field --
   ----------------------

   procedure Print_Node_Field
     (Prefix : String;
      Field  : Node_Field;
      N      : Node_Id;
      FD     : Field_Descriptor;
      Format : UI_Format := Auto)
   is
   begin
      if not Field_Is_Initial_Zero (N, Field) then
         Print_Field (Prefix, Image (Field), N, FD, Format);
      end if;
   end Print_Node_Field;

   ------------------------
   -- Print_Entity_Field --
   ------------------------

   procedure Print_Entity_Field
     (Prefix : String;
      Field  : Entity_Field;
      N      : Entity_Id;
      FD     : Field_Descriptor;
      Format : UI_Format := Auto)
   is
   begin
      if not Field_Is_Initial_Zero (N, Field) then
         Print_Field (Prefix, Image (Field), N, FD, Format);
      end if;
   end Print_Entity_Field;

   ----------------
   -- Print_Flag --
   ----------------

   procedure Print_Flag (F : Boolean) is
   begin
      if F then
         Print_Str ("True");
      else
         Print_Str ("False");
      end if;
   end Print_Flag;

   ----------------
   -- Print_Init --
   ----------------

   procedure Print_Init is
      Max_Hash_Entries : constant Nat :=
        Approx_Num_Nodes_And_Entities + Num_Lists + Num_Elists;
   begin
      Printing_Descendants := True;
      Write_Eol;

      --  Allocate and clear serial number hash table. The size is 150% of
      --  the maximum possible number of entries, so that the hash table
      --  cannot get significantly overloaded.

      Hash_Table_Len := (150 * Max_Hash_Entries) / 100;
      Hash_Table := new Hash_Table_Type  (0 .. Hash_Table_Len - 1);

      for J in Hash_Table'Range loop
         Hash_Table (J).Serial := 0;
      end loop;

   end Print_Init;

   ---------------
   -- Print_Int --
   ---------------

   procedure Print_Int (I : Int) is
   begin
      if Phase = Printing then
         Write_Int (I);
      end if;
   end Print_Int;

   --------------------
   -- Print_List_Ref --
   --------------------

   procedure Print_List_Ref (L : List_Id) is
   begin
      if Phase /= Printing then
         return;
      end if;

      if No (L) then
         Write_Str ("<no list>");

      elsif Is_Empty_List (L) then
         Write_Str ("<empty list> (List_Id=");
         Write_Int (Int (L));
         Write_Char (')');

      else
         Write_Str ("List");

         if Printing_Descendants then
            Write_Str (" #");
            Write_Int (Serial_Number (Int (L)));
         end if;

         Write_Str (" (List_Id=");
         Write_Int (Int (L));
         Write_Char (')');
      end if;
   end Print_List_Ref;

   ------------------------
   -- Print_List_Subtree --
   ------------------------

   procedure Print_List_Subtree (L : List_Id) is
   begin
      Print_Init;

      Next_Serial_Number := 1;
      Phase := Marking;
      Visit_List (L, "");

      Next_Serial_Number := 1;
      Phase := Printing;
      Visit_List (L, "");

      Print_Term;
   end Print_List_Subtree;

   ----------------
   -- Print_Name --
   ----------------

   procedure Print_Name (N : Name_Id) is
   begin
      if Phase = Printing then
         if N = No_Name then
            Print_Str ("<No_Name>");

         elsif N = Error_Name then
            Print_Str ("<Error_Name>");

         elsif Is_Valid_Name (N) then
            Get_Name_String (N);
            Print_Char ('"');
            Write_Name (N);
            Print_Char ('"');

         else
            Print_Str ("<invalid name>");
         end if;
      end if;
   end Print_Name;

   ----------------
   -- Print_Node --
   ----------------

   procedure Print_Node
     (N           : Node_Id;
      Prefix_Str  : String;
      Prefix_Char : Character)
   is
      Prefix : constant String := Prefix_Str & Prefix_Char;

      Sfile : Source_File_Index;

   begin
      if Phase /= Printing then
         return;
      end if;

      --  If there is no such node, indicate that. Skip the rest, so we don't
      --  crash getting fields of the nonexistent node.

      if not Is_Valid_Node (Union_Id (N)) then
         Print_Str ("No such node: ");
         Print_Int (Int (N));
         Print_Eol;
         return;
      end if;

      --  Print header line

      Print_Str (Prefix_Str);
      Print_Node_Header (N);

      if Is_Rewrite_Substitution (N) then
         Print_Str (Prefix_Str);
         Print_Str (" Rewritten: original node = ");
         Print_Node_Ref (Original_Node (N));
         Print_Eol;
      end if;

      if Print_Low_Level_Info then
         Print_Atree_Info (N);
      end if;

      if N = Empty then
         return;
      end if;

      if not Is_List_Member (N) then
         Print_Str (Prefix_Str);
         Print_Str (" Parent = ");
         Print_Node_Ref (Parent (N));
         Print_Eol;
      end if;

      --  Print Sloc field if it is set

      if Sloc (N) /= No_Location then
         Print_Str (Prefix);
         Print_Str ("Sloc = ");

         if Sloc (N) = Standard_Location then
            Print_Str ("Standard_Location");

         elsif Sloc (N) = Standard_ASCII_Location then
            Print_Str ("Standard_ASCII_Location");

         else
            Sfile := Get_Source_File_Index (Sloc (N));
            Print_Int (Int (Sloc (N)) - Int (Source_Text (Sfile)'First));
            Write_Str ("  ");
            Write_Location (Sloc (N));
         end if;

         Print_Eol;
      end if;

      --  Print Chars field if present

      if Nkind (N) in N_Has_Chars then
         if Field_Is_Initial_Zero (N, F_Chars) then
            Print_Str (Prefix);
            Print_Str ("Chars = initial zero");
            Print_Eol;

         elsif Chars (N) /= No_Name then
            Print_Str (Prefix);
            Print_Str ("Chars = ");
            Print_Name (Chars (N));
            Write_Str (" (Name_Id=");
            Write_Int (Int (Chars (N)));
            Write_Char (')');
            Print_Eol;
         end if;
      end if;

      --  Special field print operations for non-entity nodes

      if Nkind (N) not in N_Entity then

         --  Deal with Left_Opnd and Right_Opnd fields

         if Nkind (N) in N_Op
           or else Nkind (N) in N_Short_Circuit
           or else Nkind (N) in N_Membership_Test
         then
            --  Print Left_Opnd if present

            if Nkind (N) not in N_Unary_Op then
               Print_Str (Prefix);
               Print_Str ("Left_Opnd = ");
               Print_Node_Ref (Left_Opnd (N));
               Print_Eol;
            end if;

            --  Print Right_Opnd

            Print_Str (Prefix);
            Print_Str ("Right_Opnd = ");
            Print_Node_Ref (Right_Opnd (N));
            Print_Eol;
         end if;

         --  Deal with Entity_Or_Associated_Node. If N has both, then just
         --  print Entity; they are the same thing.

         if N in N_Inclusive_Has_Entity and then Present (Entity (N)) then
            Print_Str (Prefix);
            Print_Str ("Entity = ");
            Print_Node_Ref (Entity (N));
            Print_Eol;

         elsif N in N_Has_Associated_Node
           and then Present (Associated_Node (N))
         then
            Print_Str (Prefix);
            Print_Str ("Associated_Node = ");
            Print_Node_Ref (Associated_Node (N));
            Print_Eol;
         end if;

         --  Print special fields if we have a subexpression

         if Nkind (N) in N_Subexpr then

            if Assignment_OK (N) then
               Print_Str (Prefix);
               Print_Str ("Assignment_OK = True");
               Print_Eol;
            end if;

            if Do_Range_Check (N) then
               Print_Str (Prefix);
               Print_Str ("Do_Range_Check = True");
               Print_Eol;
            end if;

            if Has_Dynamic_Length_Check (N) then
               Print_Str (Prefix);
               Print_Str ("Has_Dynamic_Length_Check = True");
               Print_Eol;
            end if;

            if Has_Aspects (N) then
               Print_Str (Prefix);
               Print_Str ("Has_Aspects = True");
               Print_Eol;
            end if;

            if Is_Controlling_Actual (N) then
               Print_Str (Prefix);
               Print_Str ("Is_Controlling_Actual = True");
               Print_Eol;
            end if;

            if Is_Overloaded (N) then
               Print_Str (Prefix);
               Print_Str ("Is_Overloaded = True");
               Print_Eol;
            end if;

            if Is_Static_Expression (N) then
               Print_Str (Prefix);
               Print_Str ("Is_Static_Expression = True");
               Print_Eol;
            end if;

            if Must_Not_Freeze (N) then
               Print_Str (Prefix);
               Print_Str ("Must_Not_Freeze = True");
               Print_Eol;
            end if;

            if Paren_Count (N) /= 0 then
               Print_Str (Prefix);
               Print_Str ("Paren_Count = ");
               Print_Int (Int (Paren_Count (N)));
               Print_Eol;
            end if;

            if Raises_Constraint_Error (N) then
               Print_Str (Prefix);
               Print_Str ("Raises_Constraint_Error = True");
               Print_Eol;
            end if;

         end if;

         --  Print Do_Overflow_Check field if present

         if Nkind (N) in N_Op and then Do_Overflow_Check (N) then
            Print_Str (Prefix);
            Print_Str ("Do_Overflow_Check = True");
            Print_Eol;
         end if;

         --  Print Etype field if present (printing of this field for entities
         --  is handled by the Print_Entity_Info procedure).

         if Nkind (N) in N_Has_Etype and then Present (Etype (N)) then
            Print_Str (Prefix);
            Print_Str ("Etype = ");
            Print_Node_Ref (Etype (N));
            Print_Eol;
         end if;
      end if;

      declare
         Fields : Node_Field_Array renames Node_Field_Table (Nkind (N)).all;
         Should_Print : constant Node_Field_Set :=
           --  Set of fields that should be printed. False for fields that were
           --  already printed above, and for In_List, which we don't bother
           --  printing.
           (F_Nkind
            | F_Chars
            | F_Comes_From_Source
            | F_Analyzed
            | F_Error_Posted
            | F_Is_Ignored_Ghost_Node
            | F_Check_Actuals
            | F_Link -- Parent was printed
            | F_Sloc
            | F_Left_Opnd
            | F_Right_Opnd
            | F_Entity_Or_Associated_Node -- one of them was printed
            | F_Assignment_OK
            | F_Do_Range_Check
            | F_Has_Dynamic_Length_Check
            | F_Has_Aspects
            | F_Is_Controlling_Actual
            | F_Is_Overloaded
            | F_Is_Static_Expression
            | F_Must_Not_Freeze
            | F_Small_Paren_Count -- Paren_Count was printed
            | F_Raises_Constraint_Error
            | F_Do_Overflow_Check
            | F_Etype
            | F_In_List
              => False,

            others => True);

         Fmt : constant UI_Format :=
           (if Nkind (N) = N_Integer_Literal and then Print_In_Hex (N)
            then Hex
            else Auto);

      begin
         --  Outer loop makes flags come out last

         for Print_Flags in Boolean loop
            for Field_Index in Fields'Range loop
               declare
                  FD : Field_Descriptor renames
                    Field_Descriptors (Fields (Field_Index));
               begin
                  if Should_Print (Fields (Field_Index))
                    and then (FD.Kind = Flag_Field) = Print_Flags
                  then
                     --  Special case for End_Span, which also prints the
                     --  End_Location.

                     if Fields (Field_Index) = F_End_Span then
                        Print_End_Span (N);

                     else
                        Print_Node_Field
                          (Prefix, Fields (Field_Index), N, FD, Fmt);
                     end if;
                  end if;
               end;
            end loop;
         end loop;
      end;

      --  Print aspects if present

      if Has_Aspects (N) then
         Print_Str (Prefix);
         Print_Str ("Aspect_Specifications = ");
         Print_Field (Union_Id (Aspect_Specifications (N)));
         Print_Eol;
      end if;

      --  Print entity information for entities

      if Nkind (N) in N_Entity then
         Print_Entity_Info (N, Prefix);
      end if;

      --  Print the SCIL node (if available)

      if Present (Get_SCIL_Node (N)) then
         Print_Str (Prefix);
         Print_Str ("SCIL_Node = ");
         Print_Node_Ref (Get_SCIL_Node (N));
         Print_Eol;
      end if;
   end Print_Node;

   ------------------------
   -- Print_Node_Briefly --
   ------------------------

   procedure Print_Node_Briefly (N : Node_Id) is
   begin
      Printing_Descendants := False;
      Phase := Printing;
      Print_Node_Header (N);
   end Print_Node_Briefly;

   -----------------------
   -- Print_Node_Header --
   -----------------------

   procedure Print_Node_Header (N : Node_Id) is
      Enumerate : Boolean := False;
      --  Flag set when enumerating multiple header flags

      procedure Print_Header_Flag (Flag : String);
      --  Output one of the flags that appears in a node header. The routine
      --  automatically handles enumeration of multiple flags.

      -----------------------
      -- Print_Header_Flag --
      -----------------------

      procedure Print_Header_Flag (Flag : String) is
      begin
         if Enumerate then
            Print_Char (',');
         else
            Enumerate := True;
            Print_Char ('(');
         end if;

         Print_Str (Flag);
      end Print_Header_Flag;

   --  Start of processing for Print_Node_Header

   begin
      Print_Node_Ref (N);

      if not Is_Valid_Node (Union_Id (N)) then
         Print_Str (" (no such node)");
         Print_Eol;
         return;
      end if;

      Print_Char (' ');

      if Comes_From_Source (N) then
         Print_Header_Flag ("source");
      end if;

      if Analyzed (N) then
         Print_Header_Flag ("analyzed");
      end if;

      if Error_Posted (N) then
         Print_Header_Flag ("posted");
      end if;

      if Is_Ignored_Ghost_Node (N) then
         Print_Header_Flag ("ignored ghost");
      end if;

      if Check_Actuals (N) then
         Print_Header_Flag ("check actuals");
      end if;

      if Enumerate then
         Print_Char (')');
      end if;

      Print_Eol;
   end Print_Node_Header;

   ---------------------
   -- Print_Node_Kind --
   ---------------------

   procedure Print_Node_Kind (N : Node_Id) is
   begin
      if Phase = Printing then
         Print_Str_Mixed_Case (Node_Kind'Image (Nkind (N)));
      end if;
   end Print_Node_Kind;

   --------------------
   -- Print_Node_Ref --
   --------------------

   procedure Print_Node_Ref (N : Node_Id) is
      S : Nat;

   begin
      if Phase /= Printing then
         return;
      end if;

      if N = Empty then
         Write_Str ("<empty>");

      elsif N = Error then
         Write_Str ("<error>");

      else
         if Printing_Descendants then
            S := Serial_Number (Int (N));

            if S /= 0 then
               Write_Str ("Node");
               Write_Str (" #");
               Write_Int (S);
               Write_Char (' ');
            end if;
         end if;

         Print_Node_Kind (N);

         if Nkind (N) in N_Has_Chars then
            Write_Char (' ');

            if Field_Is_Initial_Zero (N, F_Chars) then
               Print_Str ("Chars = initial zero");
               Print_Eol;

            else
               Print_Name (Chars (N));
            end if;
         end if;

         if Nkind (N) in N_Entity then
            Write_Str (" (Entity_Id=");
         else
            Write_Str (" (Node_Id=");
         end if;

         Write_Int (Int (N));

         if Sloc (N) <= Standard_Location then
            Write_Char ('s');
         end if;

         Write_Char (')');

      end if;
   end Print_Node_Ref;

   ------------------------
   -- Print_Node_Subtree --
   ------------------------

   procedure Print_Node_Subtree (N : Node_Id) is
   begin
      Print_Init;

      Next_Serial_Number := 1;
      Phase := Marking;
      Visit_Node (N, "", ' ');

      Next_Serial_Number := 1;
      Phase := Printing;
      Visit_Node (N, "", ' ');

      Print_Term;
   end Print_Node_Subtree;

   ---------------
   -- Print_Str --
   ---------------

   procedure Print_Str (S : String) is
   begin
      if Phase = Printing then
         Write_Str (S);
      end if;
   end Print_Str;

   --------------------------
   -- Print_Str_Mixed_Case --
   --------------------------

   procedure Print_Str_Mixed_Case (S : String) is
      Ucase : Boolean;

   begin
      if Phase = Printing then
         Ucase := True;

         for J in S'Range loop
            if Ucase then
               Write_Char (S (J));
            else
               Write_Char (Fold_Lower (S (J)));
            end if;

            Ucase := (S (J) = '_');
         end loop;
      end if;
   end Print_Str_Mixed_Case;

   ----------------
   -- Print_Term --
   ----------------

   procedure Print_Term is
      procedure Free is new Unchecked_Deallocation
        (Hash_Table_Type, Access_Hash_Table_Type);

   begin
      Free (Hash_Table);
   end Print_Term;

   ---------------------
   -- Print_Tree_Elist --
   ---------------------

   procedure Print_Tree_Elist (E : Elist_Id) is
      M : Elmt_Id;

   begin
      Printing_Descendants := False;
      Phase := Printing;

      Print_Elist_Ref (E);
      Print_Eol;

      if Present (E) and then not Is_Empty_Elmt_List (E) then
         M := First_Elmt (E);

         loop
            Print_Char ('|');
            Print_Eol;
            exit when No (Next_Elmt (M));
            Print_Node (Node (M), "", '|');
            Next_Elmt (M);
         end loop;

         Print_Node (Node (M), "", ' ');
         Print_Eol;
      end if;
   end Print_Tree_Elist;

   ---------------------
   -- Print_Tree_List --
   ---------------------

   procedure Print_Tree_List (L : List_Id) is
      N : Node_Id;

   begin
      Printing_Descendants := False;
      Phase := Printing;

      Print_List_Ref (L);
      Print_Str (" List_Id=");
      Print_Int (Int (L));
      Print_Eol;

      N := First (L);

      if N = Empty then
         Print_Str ("<empty node list>");
         Print_Eol;

      else
         loop
            Print_Char ('|');
            Print_Eol;
            exit when Next (N) = Empty;
            Print_Node (N, "", '|');
            Next (N);
         end loop;

         Print_Node (N, "", ' ');
         Print_Eol;
      end if;
   end Print_Tree_List;

   ---------------------
   -- Print_Tree_Node --
   ---------------------

   procedure Print_Tree_Node (N : Node_Id; Label : String := "") is
   begin
      Printing_Descendants := False;
      Phase := Printing;
      Print_Node (N, Label, ' ');
   end Print_Tree_Node;

   --------
   -- pt --
   --------

   procedure pt (N : Union_Id) is
   begin
      case N is
         when List_Low_Bound .. List_High_Bound - 1 =>
            Print_List_Subtree (List_Id (N));

         when Node_Range =>
            Print_Node_Subtree (Node_Id (N));

         when Elist_Range =>
            Print_Elist_Subtree (Elist_Id (N));

         when others =>
            pp (N);
      end case;
   end pt;

   -------------------
   -- Serial_Number --
   -------------------

   --  The hashing algorithm is to use the remainder of the ID value divided
   --  by the hash table length as the starting point in the table, and then
   --  handle collisions by serial searching wrapping at the end of the table.

   Hash_Slot : Nat;
   --  Set by an unsuccessful call to Serial_Number (one which returns zero)
   --  to save the slot that should be used if Set_Serial_Number is called.

   function Serial_Number (Id : Int) return Nat is
      H : Int := Id mod Hash_Table_Len;

   begin
      while Hash_Table (H).Serial /= 0 loop

         if Id = Hash_Table (H).Id then
            return Hash_Table (H).Serial;
         end if;

         H := H + 1;

         if H > Hash_Table'Last then
            H := 0;
         end if;
      end loop;

      --  Entry was not found, save slot number for possible subsequent call
      --  to Set_Serial_Number, and unconditionally save the Id in this slot
      --  in case of such a call (the Id field is never read if the serial
      --  number of the slot is zero, so this is harmless in the case where
      --  Set_Serial_Number is not subsequently called).

      Hash_Slot := H;
      Hash_Table (H).Id := Id;
      return 0;
   end Serial_Number;

   -----------------------
   -- Set_Serial_Number --
   -----------------------

   procedure Set_Serial_Number is
   begin
      Hash_Table (Hash_Slot).Serial := Next_Serial_Number;
      Next_Serial_Number := Next_Serial_Number + 1;
   end Set_Serial_Number;

   ---------------
   -- Tree_Dump --
   ---------------

   procedure Tree_Dump is
      procedure Underline;
      --  Put underline under string we just printed

      procedure Underline is
         Col : constant Int := Column;

      begin
         Write_Eol;

         while Col > Column loop
            Write_Char ('-');
         end loop;

         Write_Eol;
      end Underline;

   --  Start of processing for Tree_Dump. Note that we turn off the tree dump
   --  flags immediately, before starting the dump. This avoids generating two
   --  copies of the dump if an abort occurs after printing the dump, and more
   --  importantly, avoids an infinite loop if an abort occurs during the dump.

   --  Note: unlike in the source print case (in Sprint), we do not output
   --  separate trees for each unit. Instead the -df debug switch causes the
   --  tree that is output from the main unit to trace references into other
   --  units (normally such references are not traced). Since all other units
   --  are linked to the main unit by at least one reference, this causes all
   --  tree nodes to be included in the output tree.

   begin
      if Debug_Flag_Y then
         Debug_Flag_Y := False;
         Write_Eol;
         Write_Str ("Tree created for Standard (spec) ");
         Underline;
         Print_Node_Subtree (Standard_Package_Node);
         Write_Eol;
      end if;

      if Debug_Flag_T then
         Debug_Flag_T := False;

         Write_Eol;
         Write_Str ("Tree created for ");
         Write_Unit_Name (Unit_Name (Main_Unit));
         Underline;
         Print_Node_Subtree (Cunit (Main_Unit));
         Write_Eol;
      end if;
   end Tree_Dump;

   -----------------
   -- Visit_Elist --
   -----------------

   procedure Visit_Elist (E : Elist_Id; Prefix_Str : String) is
      M : Elmt_Id;
      N : Node_Id;
      S : constant Nat := Serial_Number (Int (E));

   begin
      --  In marking phase, return if already marked, otherwise set next
      --  serial number in hash table for later reference.

      if Phase = Marking then
         if S /= 0 then
            return; -- already visited
         else
            Set_Serial_Number;
         end if;

      --  In printing phase, if already printed, then return, otherwise we
      --  are printing the next item, so increment the serial number.

      else
         if S < Next_Serial_Number then
            return; -- already printed
         else
            Next_Serial_Number := Next_Serial_Number + 1;
         end if;
      end if;

      --  Now process the list (Print calls have no effect in marking phase)

      Print_Str (Prefix_Str);
      Print_Elist_Ref (E);
      Print_Eol;

      if Is_Empty_Elmt_List (E) then
         Print_Str (Prefix_Str);
         Print_Str ("(Empty element list)");
         Print_Eol;
         Print_Eol;

      else
         if Phase = Printing then
            M := First_Elmt (E);
            while Present (M) loop
               N := Node (M);
               Print_Str (Prefix_Str);
               Print_Str (" ");
               Print_Node_Ref (N);
               Print_Eol;
               Next_Elmt (M);
            end loop;

            Print_Str (Prefix_Str);
            Print_Eol;
         end if;

         M := First_Elmt (E);
         while Present (M) loop
            Visit_Node (Node (M), Prefix_Str, ' ');
            Next_Elmt (M);
         end loop;
      end if;
   end Visit_Elist;

   ----------------
   -- Visit_List --
   ----------------

   procedure Visit_List (L : List_Id; Prefix_Str : String) is
      N : Node_Id;
      S : constant Nat := Serial_Number (Int (L));

   begin
      --  In marking phase, return if already marked, otherwise set next
      --  serial number in hash table for later reference.

      if Phase = Marking then
         if S /= 0 then
            return;
         else
            Set_Serial_Number;
         end if;

      --  In printing phase, if already printed, then return, otherwise we
      --  are printing the next item, so increment the serial number.

      else
         if S < Next_Serial_Number then
            return; -- already printed
         else
            Next_Serial_Number := Next_Serial_Number + 1;
         end if;
      end if;

      --  Now process the list (Print calls have no effect in marking phase)

      Print_Str (Prefix_Str);
      Print_List_Ref (L);
      Print_Eol;

      Print_Str (Prefix_Str);
      Print_Str ("|Parent = ");
      Print_Node_Ref (Parent (L));
      Print_Eol;

      N := First (L);

      if N = Empty then
         Print_Str (Prefix_Str);
         Print_Str ("(Empty list)");
         Print_Eol;
         Print_Eol;

      else
         Print_Str (Prefix_Str);
         Print_Char ('|');
         Print_Eol;

         while Next (N) /= Empty loop
            Visit_Node (N, Prefix_Str, '|');
            Next (N);
         end loop;
      end if;

      Visit_Node (N, Prefix_Str, ' ');
   end Visit_List;

   ----------------
   -- Visit_Node --
   ----------------

   procedure Visit_Node
     (N           : Node_Id;
      Prefix_Str  : String;
      Prefix_Char : Character)
   is
      New_Prefix : String (Prefix_Str'First .. Prefix_Str'Last + 2);
      --  Prefix string for printing referenced fields

      procedure Visit_Descendant (D : Union_Id);
      --  This procedure tests the given value of one of the Fields referenced
      --  by the current node to determine whether to visit it recursively.
      --  The visited node will be indented using New_Prefix.

      ----------------------
      -- Visit_Descendant --
      ----------------------

      procedure Visit_Descendant (D : Union_Id) is
      begin
         --  Case of descendant is a node

         if D in Node_Range then

            --  Don't bother about Empty or Error descendants

            if D <= Union_Id (Empty_Or_Error) then
               return;
            end if;

            declare
               Nod : constant Node_Or_Entity_Id := Node_Or_Entity_Id (D);

            begin
               --  Descendants in one of the standardly compiled internal
               --  packages are normally ignored, unless the parent is also
               --  in such a package (happens when Standard itself is output)
               --  or if the -df switch is set which causes all links to be
               --  followed, even into package standard.

               if Sloc (Nod) <= Standard_Location then
                  if Sloc (N) > Standard_Location
                    and then not Debug_Flag_F
                  then
                     return;
                  end if;

               --  Don't bother about a descendant in a different unit than
               --  the node we came from unless the -df switch is set. Note
               --  that we know at this point that Sloc (D) > Standard_Location

               --  Note: the tests for No_Location here just make sure that we
               --  don't blow up on a node which is missing an Sloc value. This
               --  should not normally happen.

               else
                  if (Sloc (N) <= Standard_Location
                        or else Sloc (N) = No_Location
                        or else Sloc (Nod) = No_Location
                        or else not In_Same_Source_Unit (Nod, N))
                    and then not Debug_Flag_F
                  then
                     return;
                  end if;
               end if;

               --  Don't bother visiting a source node that has a parent which
               --  is not the node we came from. We prefer to trace such nodes
               --  from their real parents. This causes the tree to be printed
               --  in a more coherent order, e.g. a defining identifier listed
               --  next to its corresponding declaration, instead of next to
               --  some semantic reference.

               --  This test is skipped for nodes in standard packages unless
               --  the -dy option is set (which outputs the tree for standard)

               --  Also, always follow pointers to Is_Itype entities,
               --  since we want to list these when they are first referenced.

               if Parent (Nod) /= Empty
                 and then Comes_From_Source (Nod)
                 and then Parent (Nod) /= N
                 and then (Sloc (N) > Standard_Location or else Debug_Flag_Y)
               then
                  return;
               end if;

               --  If we successfully fall through all the above tests (which
               --  execute a return if the node is not to be visited), we can
               --  go ahead and visit the node.

               Visit_Node (Nod, New_Prefix, ' ');
            end;

         --  Case of descendant is a list

         elsif D in List_Range then

            --  Don't bother with a missing list, empty list or error list

            pragma Assert (D /= Union_Id (No_List));
            --  Because No_List = Empty, which is in Node_Range above

            if D = Union_Id (Error_List)
              or else Is_Empty_List (List_Id (D))
            then
               return;

            --  Otherwise we can visit the list. Note that we don't bother to
            --  do the parent test that we did for the node case, because it
            --  just does not happen that lists are referenced more than one
            --  place in the tree. We aren't counting on this being the case
            --  to generate valid output, it is just that we don't need in
            --  practice to worry about listing the list at a place that is
            --  inconvenient.

            else
               Visit_List (List_Id (D), New_Prefix);
            end if;

         --  Case of descendant is an element list

         elsif D in Elist_Range then

            --  Don't bother with a missing list, or an empty list

            if D = Union_Id (No_Elist)
              or else Is_Empty_Elmt_List (Elist_Id (D))
            then
               return;

            --  Otherwise, visit the referenced element list

            else
               Visit_Elist (Elist_Id (D), New_Prefix);
            end if;

         else
            raise Program_Error;
         end if;
      end Visit_Descendant;

   --  Start of processing for Visit_Node

   begin
      if N = Empty then
         return;
      end if;

      --  Set fatal error node in case we get a blow up during the trace

      Current_Error_Node := N;

      New_Prefix (Prefix_Str'Range)    := Prefix_Str;
      New_Prefix (Prefix_Str'Last + 1) := Prefix_Char;
      New_Prefix (Prefix_Str'Last + 2) := ' ';

      --  In the marking phase, all we do is to set the serial number

      if Phase = Marking then
         if Serial_Number (Int (N)) /= 0 then
            return; -- already visited
         else
            Set_Serial_Number;
         end if;

      --  In the printing phase, we print the node

      else
         if Serial_Number (Int (N)) < Next_Serial_Number then

            --  Here we have already visited the node, but if it is in a list,
            --  we still want to print the reference, so that it is clear that
            --  it belongs to the list.

            if Is_List_Member (N) then
               Print_Str (Prefix_Str);
               Print_Node_Ref (N);
               Print_Eol;
               Print_Str (Prefix_Str);
               Print_Char (Prefix_Char);
               Print_Str ("(already output)");
               Print_Eol;
               Print_Str (Prefix_Str);
               Print_Char (Prefix_Char);
               Print_Eol;
            end if;

            return;

         else
            Print_Node (N, Prefix_Str, Prefix_Char);
            Print_Str (Prefix_Str);
            Print_Char (Prefix_Char);
            Print_Eol;
            Next_Serial_Number := Next_Serial_Number + 1;
         end if;
      end if;

      --  Visit all descendants of this node

      declare
         A : Node_Field_Array renames Node_Field_Table (Nkind (N)).all;
      begin
         for Field_Index in A'Range loop
            declare
               F : constant Node_Field := A (Field_Index);
               FD : Field_Descriptor renames Field_Descriptors (F);
            begin
               if FD.Kind in Node_Id_Field | List_Id_Field | Elist_Id_Field
                  --  For all other kinds of descendants (strings, names, uints
                  --  etc), there is nothing to visit (the contents of the
                  --  field will be printed when we print the containing node,
                  --  but what concerns us now is looking for descendants in
                  --  the tree.

                 and then F /= F_Next_Entity -- See below for why we skip this
               then
                  Visit_Descendant (Get_Union_Id (N, FD.Offset));
               end if;
            end;
         end loop;
      end;

      if Has_Aspects (N) then
         Visit_Descendant (Union_Id (Aspect_Specifications (N)));
      end if;

      if Nkind (N) in N_Entity then
         declare
            A : Entity_Field_Array renames Entity_Field_Table (Ekind (N)).all;
         begin
            for Field_Index in A'Range loop
               declare
                  F : constant Entity_Field := A (Field_Index);
                  FD : Field_Descriptor renames Field_Descriptors (F);
               begin
                  if FD.Kind in Node_Id_Field | List_Id_Field | Elist_Id_Field
                  then
                     Visit_Descendant (Get_Union_Id (N, FD.Offset));
                  end if;
               end;
            end loop;
         end;

         --  Now an interesting special case. Normally parents are always
         --  printed since we traverse the tree in a downwards direction.
         --  However, there is an exception to this rule, which is the
         --  case where a parent is constructed by the compiler and is not
         --  referenced elsewhere in the tree. The following catches this case.

         if not Comes_From_Source (N) then
            Visit_Descendant (Union_Id (Parent (N)));
         end if;

         --  You may be wondering why we omitted Next_Entity above. The answer
         --  is that we want to treat it rather specially. Why? Because a
         --  Next_Entity link does not correspond to a level deeper in the
         --  tree, and we do not want the tree to march off to the right of the
         --  page due to bogus indentations coming from this effect.

         --  To prevent this, what we do is to control references via
         --  Next_Entity only from the first entity on a given scope chain,
         --  and we keep them all at the same level. Of course if an entity
         --  has already been referenced it is not printed.

         if Present (Next_Entity (N))
           and then Present (Scope (N))
           and then First_Entity (Scope (N)) = N
         then
            declare
               Nod : Node_Id;

            begin
               Nod := N;
               while Present (Nod) loop
                  Visit_Descendant (Union_Id (Next_Entity (Nod)));
                  Next_Entity (Nod);
               end loop;
            end;
         end if;
      end if;
   end Visit_Node;

end Treepr;
