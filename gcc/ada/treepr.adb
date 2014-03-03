------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               T R E E P R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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

with Aspects;  use Aspects;
with Atree;    use Atree;
with Csets;    use Csets;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Output;   use Output;
with Sem_Mech; use Sem_Mech;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Sinput;   use Sinput;
with Stand;    use Stand;
with Stringt;  use Stringt;
with SCIL_LL;  use SCIL_LL;
with Treeprs;  use Treeprs;
with Uintp;    use Uintp;
with Urealp;   use Urealp;
with Uname;    use Uname;
with Unchecked_Deallocation;

package body Treepr is

   use Atree.Unchecked_Access;
   --  This module uses the unchecked access functions in package Atree
   --  since it does an untyped traversal of the tree (we do not want to
   --  count on the structure of the tree being correct in this routine).

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

   procedure Print_End_Span (N : Node_Id);
   --  Special routine to print contents of End_Span field of node N.
   --  The format includes the implicit source location as well as the
   --  value of the field.

   procedure Print_Init;
   --  Initialize for printing of tree with descendents

   procedure Print_Term;
   --  Clean up after printing of tree with descendents

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
   --  Print representation of Field value (name, tree, string, uint, charcode)
   --  The format parameter controls the format of printing in the case of an
   --  integer value (see UI_Write for details).

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
   --  Called to process a single node in the case where descendents are to
   --  be printed before every line, and Prefix_Char added to all lines
   --  except the header line for the node.

   procedure Visit_List (L : List_Id; Prefix_Str : String);
   --  Visit_List is called to process a list in the case where descendents
   --  are to be printed. Prefix_Str is to be added to all printed lines.

   procedure Visit_Elist (E : Elist_Id; Prefix_Str : String);
   --  Visit_Elist is called to process an element list in the case where
   --  descendents are to be printed. Prefix_Str is to be added to all
   --  printed lines.

   -------
   -- p --
   -------

   function p (N : Union_Id) return Node_Or_Entity_Id is
   begin
      case N is
         when List_Low_Bound .. List_High_Bound - 1 =>
            return Nlists.Parent (List_Id (N));

         when Node_Range =>
            return Atree.Parent (Node_Or_Entity_Id (N));

         when others =>
            Write_Int (Int (N));
            Write_Str (" is not a Node_Id or List_Id value");
            Write_Eol;
            return Empty;
      end case;
   end p;

   --------
   -- pe --
   --------

   procedure pe (E : Elist_Id) is
   begin
      Print_Tree_Elist (E);
   end pe;

   --------
   -- pl --
   --------

   procedure pl (L : Int) is
      Lid : Int;

   begin
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
   end pl;

   --------
   -- pn --
   --------

   procedure pn (N : Union_Id) is
   begin
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
   end pn;

   --------
   -- pp --
   --------

   procedure pp (N : Union_Id) is
   begin
      pn (N);
   end pp;

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
      Write_Int (Int (Field5 (N)));
      Write_Str (")  ");

      if Val /= No_Uint then
         Write_Location (End_Location (N));
      end if;
   end Print_End_Span;

   -----------------------
   -- Print_Entity_Info --
   -----------------------

   procedure Print_Entity_Info (Ent : Entity_Id; Prefix : String) is
      function Field_Present (U : Union_Id) return Boolean;
      --  Returns False unless the value U represents a missing value
      --  (Empty, No_Uint, No_Ureal or No_String)

      function Field_Present (U : Union_Id) return Boolean is
      begin
         return
            U /= Union_Id (Empty)    and then
            U /= To_Union (No_Uint)  and then
            U /= To_Union (No_Ureal) and then
            U /= Union_Id (No_String);
      end Field_Present;

   --  Start of processing for Print_Entity_Info

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

      if Field_Present (Field6 (Ent)) then
         Print_Str (Prefix);
         Write_Field6_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field6 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field7 (Ent)) then
         Print_Str (Prefix);
         Write_Field7_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field7 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field8 (Ent)) then
         Print_Str (Prefix);
         Write_Field8_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field8 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field9 (Ent)) then
         Print_Str (Prefix);
         Write_Field9_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field9 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field10 (Ent)) then
         Print_Str (Prefix);
         Write_Field10_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field10 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field11 (Ent)) then
         Print_Str (Prefix);
         Write_Field11_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field11 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field12 (Ent)) then
         Print_Str (Prefix);
         Write_Field12_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field12 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field13 (Ent)) then
         Print_Str (Prefix);
         Write_Field13_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field13 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field14 (Ent)) then
         Print_Str (Prefix);
         Write_Field14_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field14 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field15 (Ent)) then
         Print_Str (Prefix);
         Write_Field15_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field15 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field16 (Ent)) then
         Print_Str (Prefix);
         Write_Field16_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field16 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field17 (Ent)) then
         Print_Str (Prefix);
         Write_Field17_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field17 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field18 (Ent)) then
         Print_Str (Prefix);
         Write_Field18_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field18 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field19 (Ent)) then
         Print_Str (Prefix);
         Write_Field19_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field19 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field20 (Ent)) then
         Print_Str (Prefix);
         Write_Field20_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field20 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field21 (Ent)) then
         Print_Str (Prefix);
         Write_Field21_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field21 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field22 (Ent)) then
         Print_Str (Prefix);
         Write_Field22_Name (Ent);
         Write_Str (" = ");

         --  Mechanism case has to be handled specially

         if Ekind (Ent) = E_Function or else Is_Formal (Ent) then
            declare
               M : constant Mechanism_Type := Mechanism (Ent);

            begin
               case M is
                  when Default_Mechanism
                                    => Write_Str ("Default");
                  when By_Copy
                                    => Write_Str ("By_Copy");
                  when By_Reference
                                    => Write_Str ("By_Reference");
                  when By_Descriptor
                                    => Write_Str ("By_Descriptor");
                  when By_Descriptor_UBS
                                    => Write_Str ("By_Descriptor_UBS");
                  when By_Descriptor_UBSB
                                    => Write_Str ("By_Descriptor_UBSB");
                  when By_Descriptor_UBA
                                    => Write_Str ("By_Descriptor_UBA");
                  when By_Descriptor_S
                                    => Write_Str ("By_Descriptor_S");
                  when By_Descriptor_SB
                                    => Write_Str ("By_Descriptor_SB");
                  when By_Descriptor_A
                                    => Write_Str ("By_Descriptor_A");
                  when By_Descriptor_NCA
                                    => Write_Str ("By_Descriptor_NCA");
                  when By_Short_Descriptor
                                    => Write_Str ("By_Short_Descriptor");
                  when By_Short_Descriptor_UBS
                                    => Write_Str ("By_Short_Descriptor_UBS");
                  when By_Short_Descriptor_UBSB
                                    => Write_Str ("By_Short_Descriptor_UBSB");
                  when By_Short_Descriptor_UBA
                                    => Write_Str ("By_Short_Descriptor_UBA");
                  when By_Short_Descriptor_S
                                    => Write_Str ("By_Short_Descriptor_S");
                  when By_Short_Descriptor_SB
                                    => Write_Str ("By_Short_Descriptor_SB");
                  when By_Short_Descriptor_A
                                    => Write_Str ("By_Short_Descriptor_A");
                  when By_Short_Descriptor_NCA
                                    => Write_Str ("By_Short_Descriptor_NCA");

                  when 1 .. Mechanism_Type'Last =>
                     Write_Str ("By_Copy if size <= ");
                     Write_Int (Int (M));

               end case;
            end;

         --  Normal case (not Mechanism)

         else
            Print_Field (Field22 (Ent));
         end if;

         Print_Eol;
      end if;

      if Field_Present (Field23 (Ent)) then
         Print_Str (Prefix);
         Write_Field23_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field23 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field24 (Ent)) then
         Print_Str (Prefix);
         Write_Field24_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field24 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field25 (Ent)) then
         Print_Str (Prefix);
         Write_Field25_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field25 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field26 (Ent)) then
         Print_Str (Prefix);
         Write_Field26_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field26 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field27 (Ent)) then
         Print_Str (Prefix);
         Write_Field27_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field27 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field28 (Ent)) then
         Print_Str (Prefix);
         Write_Field28_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field28 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field29 (Ent)) then
         Print_Str (Prefix);
         Write_Field29_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field29 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field30 (Ent)) then
         Print_Str (Prefix);
         Write_Field30_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field30 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field31 (Ent)) then
         Print_Str (Prefix);
         Write_Field31_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field31 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field32 (Ent)) then
         Print_Str (Prefix);
         Write_Field32_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field32 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field33 (Ent)) then
         Print_Str (Prefix);
         Write_Field33_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field33 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field34 (Ent)) then
         Print_Str (Prefix);
         Write_Field34_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field34 (Ent));
         Print_Eol;
      end if;

      if Field_Present (Field35 (Ent)) then
         Print_Str (Prefix);
         Write_Field35_Name (Ent);
         Write_Str (" = ");
         Print_Field (Field35 (Ent));
         Print_Eol;
      end if;

      Write_Entity_Flags (Ent, Prefix);
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
   begin
      Printing_Descendants := True;
      Write_Eol;

      --  Allocate and clear serial number hash table. The size is 150% of
      --  the maximum possible number of entries, so that the hash table
      --  cannot get significantly overloaded.

      Hash_Table_Len := (150 * (Num_Nodes + Num_Lists + Num_Elists)) / 100;
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
            Print_Str ("<invalid name ???>");
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
      F : Fchar;
      P : Natural := Pchar_Pos (Nkind (N));

      Field_To_Be_Printed : Boolean;
      Prefix_Str_Char     : String (Prefix_Str'First .. Prefix_Str'Last + 1);

      Sfile : Source_File_Index;
      Fmt   : UI_Format;

   begin
      if Phase /= Printing then
         return;
      end if;

      if Nkind (N) = N_Integer_Literal and then Print_In_Hex (N) then
         Fmt := Hex;
      else
         Fmt := Auto;
      end if;

      Prefix_Str_Char (Prefix_Str'Range)    := Prefix_Str;
      Prefix_Str_Char (Prefix_Str'Last + 1) := Prefix_Char;

      --  Print header line

      Print_Str (Prefix_Str);
      Print_Node_Header (N);

      if Is_Rewrite_Substitution (N) then
         Print_Str (Prefix_Str);
         Print_Str (" Rewritten: original node = ");
         Print_Node_Ref (Original_Node (N));
         Print_Eol;
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
         Print_Str (Prefix_Str_Char);
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

      if Nkind (N) in N_Has_Chars and then Chars (N) /= No_Name then
         Print_Str (Prefix_Str_Char);
         Print_Str ("Chars = ");
         Print_Name (Chars (N));
         Write_Str (" (Name_Id=");
         Write_Int (Int (Chars (N)));
         Write_Char (')');
         Print_Eol;
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
               Print_Str (Prefix_Str_Char);
               Print_Str ("Left_Opnd = ");
               Print_Node_Ref (Left_Opnd (N));
               Print_Eol;
            end if;

            --  Print Right_Opnd

            Print_Str (Prefix_Str_Char);
            Print_Str ("Right_Opnd = ");
            Print_Node_Ref (Right_Opnd (N));
            Print_Eol;
         end if;

         --  Print Entity field if operator (other cases of Entity
         --  are in the table, so are handled in the normal circuit)

         if Nkind (N) in N_Op and then Present (Entity (N)) then
            Print_Str (Prefix_Str_Char);
            Print_Str ("Entity = ");
            Print_Node_Ref (Entity (N));
            Print_Eol;
         end if;

         --  Print special fields if we have a subexpression

         if Nkind (N) in N_Subexpr then

            if Assignment_OK (N) then
               Print_Str (Prefix_Str_Char);
               Print_Str ("Assignment_OK = True");
               Print_Eol;
            end if;

            if Do_Range_Check (N) then
               Print_Str (Prefix_Str_Char);
               Print_Str ("Do_Range_Check = True");
               Print_Eol;
            end if;

            if Has_Dynamic_Length_Check (N) then
               Print_Str (Prefix_Str_Char);
               Print_Str ("Has_Dynamic_Length_Check = True");
               Print_Eol;
            end if;

            if Has_Aspects (N) then
               Print_Str (Prefix_Str_Char);
               Print_Str ("Has_Aspects = True");
               Print_Eol;
            end if;

            if Has_Dynamic_Range_Check (N) then
               Print_Str (Prefix_Str_Char);
               Print_Str ("Has_Dynamic_Range_Check = True");
               Print_Eol;
            end if;

            if Is_Controlling_Actual (N) then
               Print_Str (Prefix_Str_Char);
               Print_Str ("Is_Controlling_Actual = True");
               Print_Eol;
            end if;

            if Is_Overloaded (N) then
               Print_Str (Prefix_Str_Char);
               Print_Str ("Is_Overloaded = True");
               Print_Eol;
            end if;

            if Is_Static_Expression (N) then
               Print_Str (Prefix_Str_Char);
               Print_Str ("Is_Static_Expression = True");
               Print_Eol;
            end if;

            if Must_Not_Freeze (N) then
               Print_Str (Prefix_Str_Char);
               Print_Str ("Must_Not_Freeze = True");
               Print_Eol;
            end if;

            if Paren_Count (N) /= 0 then
               Print_Str (Prefix_Str_Char);
               Print_Str ("Paren_Count = ");
               Print_Int (Int (Paren_Count (N)));
               Print_Eol;
            end if;

            if Raises_Constraint_Error (N) then
               Print_Str (Prefix_Str_Char);
               Print_Str ("Raise_Constraint_Error = True");
               Print_Eol;
            end if;

         end if;

         --  Print Do_Overflow_Check field if present

         if Nkind (N) in N_Op and then Do_Overflow_Check (N) then
            Print_Str (Prefix_Str_Char);
            Print_Str ("Do_Overflow_Check = True");
            Print_Eol;
         end if;

         --  Print Etype field if present (printing of this field for entities
         --  is handled by the Print_Entity_Info procedure).

         if Nkind (N) in N_Has_Etype and then Present (Etype (N)) then
            Print_Str (Prefix_Str_Char);
            Print_Str ("Etype = ");
            Print_Node_Ref (Etype (N));
            Print_Eol;
         end if;
      end if;

      --  Loop to print fields included in Pchars array

      while P < Pchar_Pos (Node_Kind'Succ (Nkind (N))) loop
         F := Pchars (P);
         P := P + 1;

         --  Check for case of False flag, which we never print, or
         --  an Empty field, which is also never printed

         case F is
            when F_Field1 =>
               Field_To_Be_Printed := Field1 (N) /= Union_Id (Empty);

            when F_Field2 =>
               Field_To_Be_Printed := Field2 (N) /= Union_Id (Empty);

            when F_Field3 =>
               Field_To_Be_Printed := Field3 (N) /= Union_Id (Empty);

            when F_Field4 =>
               Field_To_Be_Printed := Field4 (N) /= Union_Id (Empty);

            when F_Field5 =>
               Field_To_Be_Printed := Field5 (N) /= Union_Id (Empty);

            when F_Flag1  => Field_To_Be_Printed := Flag1  (N);
            when F_Flag2  => Field_To_Be_Printed := Flag2  (N);
            when F_Flag3  => Field_To_Be_Printed := Flag3  (N);
            when F_Flag4  => Field_To_Be_Printed := Flag4  (N);
            when F_Flag5  => Field_To_Be_Printed := Flag5  (N);
            when F_Flag6  => Field_To_Be_Printed := Flag6  (N);
            when F_Flag7  => Field_To_Be_Printed := Flag7  (N);
            when F_Flag8  => Field_To_Be_Printed := Flag8  (N);
            when F_Flag9  => Field_To_Be_Printed := Flag9  (N);
            when F_Flag10 => Field_To_Be_Printed := Flag10 (N);
            when F_Flag11 => Field_To_Be_Printed := Flag11 (N);
            when F_Flag12 => Field_To_Be_Printed := Flag12 (N);
            when F_Flag13 => Field_To_Be_Printed := Flag13 (N);
            when F_Flag14 => Field_To_Be_Printed := Flag14 (N);
            when F_Flag15 => Field_To_Be_Printed := Flag15 (N);
            when F_Flag16 => Field_To_Be_Printed := Flag16 (N);
            when F_Flag17 => Field_To_Be_Printed := Flag17 (N);
            when F_Flag18 => Field_To_Be_Printed := Flag18 (N);
         end case;

         --  Print field if it is to be printed

         if Field_To_Be_Printed then
            Print_Str (Prefix_Str_Char);

            while P < Pchar_Pos (Node_Kind'Succ (Nkind (N)))
              and then Pchars (P) not in Fchar
            loop
               Print_Char (Pchars (P));
               P := P + 1;
            end loop;

            Print_Str (" = ");

            case F is
               when F_Field1 => Print_Field (Field1 (N), Fmt);
               when F_Field2 => Print_Field (Field2 (N), Fmt);
               when F_Field3 => Print_Field (Field3 (N), Fmt);
               when F_Field4 => Print_Field (Field4 (N), Fmt);

               --  Special case End_Span = Uint5

               when F_Field5 =>
                  if Nkind_In (N, N_Case_Statement, N_If_Statement) then
                     Print_End_Span (N);
                  else
                     Print_Field (Field5 (N), Fmt);
                  end if;

               when F_Flag1  => Print_Flag  (Flag1 (N));
               when F_Flag2  => Print_Flag  (Flag2 (N));
               when F_Flag3  => Print_Flag  (Flag3 (N));
               when F_Flag4  => Print_Flag  (Flag4 (N));
               when F_Flag5  => Print_Flag  (Flag5 (N));
               when F_Flag6  => Print_Flag  (Flag6 (N));
               when F_Flag7  => Print_Flag  (Flag7 (N));
               when F_Flag8  => Print_Flag  (Flag8 (N));
               when F_Flag9  => Print_Flag  (Flag9 (N));
               when F_Flag10 => Print_Flag  (Flag10 (N));
               when F_Flag11 => Print_Flag  (Flag11 (N));
               when F_Flag12 => Print_Flag  (Flag12 (N));
               when F_Flag13 => Print_Flag  (Flag13 (N));
               when F_Flag14 => Print_Flag  (Flag14 (N));
               when F_Flag15 => Print_Flag  (Flag15 (N));
               when F_Flag16 => Print_Flag  (Flag16 (N));
               when F_Flag17 => Print_Flag  (Flag17 (N));
               when F_Flag18 => Print_Flag  (Flag18 (N));
            end case;

            Print_Eol;

         --  Field is not to be printed (False flag field)

         else
            while P < Pchar_Pos (Node_Kind'Succ (Nkind (N)))
              and then Pchars (P) not in Fchar
            loop
               P := P + 1;
            end loop;
         end if;
      end loop;

      --  Print aspects if present

      if Has_Aspects (N) then
         Print_Str (Prefix_Str_Char);
         Print_Str ("Aspect_Specifications = ");
         Print_Field (Union_Id (Aspect_Specifications (N)));
         Print_Eol;
      end if;

      --  Print entity information for entities

      if Nkind (N) in N_Entity then
         Print_Entity_Info (N, Prefix_Str_Char);
      end if;

      --  Print the SCIL node (if available)

      if Present (Get_SCIL_Node (N)) then
         Print_Str (Prefix_Str_Char);
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
      Notes : Boolean := False;

   begin
      Print_Node_Ref (N);

      if N > Atree_Private_Part.Nodes.Last then
         Print_Str (" (no such node)");
         Print_Eol;
         return;
      end if;

      if Comes_From_Source (N) then
         Notes := True;
         Print_Str (" (source");
      end if;

      if Analyzed (N) then
         if not Notes then
            Notes := True;
            Print_Str (" (");
         else
            Print_Str (",");
         end if;

         Print_Str ("analyzed");
      end if;

      if Error_Posted (N) then
         if not Notes then
            Notes := True;
            Print_Str (" (");
         else
            Print_Str (",");
         end if;

         Print_Str ("posted");
      end if;

      if Notes then
         Print_Char (')');
      end if;

      Print_Eol;
   end Print_Node_Header;

   ---------------------
   -- Print_Node_Kind --
   ---------------------

   procedure Print_Node_Kind (N : Node_Id) is
      Ucase : Boolean;
      S     : constant String := Node_Kind'Image (Nkind (N));

   begin
      if Phase = Printing then
         Ucase := True;

         --  Note: the call to Fold_Upper in this loop is to get past the GNAT
         --  bug of 'Image returning lower case instead of upper case.

         for J in S'Range loop
            if Ucase then
               Write_Char (Fold_Upper (S (J)));
            else
               Write_Char (Fold_Lower (S (J)));
            end if;

            Ucase := (S (J) = '_');
         end loop;
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
            Print_Name (Chars (N));
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

      M := First_Elmt (E);

      if No (M) then
         Print_Str ("<empty element list>");
         Print_Eol;

      else
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

   procedure pt (N : Node_Id) is
   begin
      Print_Node_Subtree (N);
   end pt;

   ---------
   -- ppp --
   ---------

   procedure ppp (N : Node_Id) is
   begin
      pt (N);
   end ppp;

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

      procedure Visit_Descendent
        (D         : Union_Id;
         No_Indent : Boolean := False);
      --  This procedure tests the given value of one of the Fields referenced
      --  by the current node to determine whether to visit it recursively.
      --  Normally No_Indent is false, which means that the visited node will
      --  be indented using New_Prefix. If No_Indent is set to True, then
      --  this indentation is skipped, and Prefix_Str is used for the call
      --  to print the descendent. No_Indent is effective only if the
      --  referenced descendent is a node.

      ----------------------
      -- Visit_Descendent --
      ----------------------

      procedure Visit_Descendent
        (D         : Union_Id;
         No_Indent : Boolean := False)
      is
      begin
         --  Case of descendent is a node

         if D in Node_Range then

            --  Don't bother about Empty or Error descendents

            if D <= Union_Id (Empty_Or_Error) then
               return;
            end if;

            declare
               Nod : constant Node_Or_Entity_Id := Node_Or_Entity_Id (D);

            begin
               --  Descendents in one of the standardly compiled internal
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

               --  Don't bother about a descendent in a different unit than
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

               if No_Indent then
                  Visit_Node (Nod, Prefix_Str, Prefix_Char);
               else
                  Visit_Node (Nod, New_Prefix, ' ');
               end if;
            end;

         --  Case of descendent is a list

         elsif D in List_Range then

            --  Don't bother with a missing list, empty list or error list

            if D = Union_Id (No_List)
              or else D = Union_Id (Error_List)
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

         --  Case of descendent is an element list

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

         --  For all other kinds of descendents (strings, names, uints etc),
         --  there is nothing to visit (the contents of the field will be
         --  printed when we print the containing node, but what concerns
         --  us now is looking for descendents in the tree.

         else
            null;
         end if;
      end Visit_Descendent;

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

      --  Visit all descendents of this node

      if Nkind (N) not in N_Entity then
         Visit_Descendent (Field1 (N));
         Visit_Descendent (Field2 (N));
         Visit_Descendent (Field3 (N));
         Visit_Descendent (Field4 (N));
         Visit_Descendent (Field5 (N));

         if Has_Aspects (N) then
            Visit_Descendent (Union_Id (Aspect_Specifications (N)));
         end if;

      --  Entity case

      else
         Visit_Descendent (Field1 (N));
         Visit_Descendent (Field3 (N));
         Visit_Descendent (Field4 (N));
         Visit_Descendent (Field5 (N));
         Visit_Descendent (Field6 (N));
         Visit_Descendent (Field7 (N));
         Visit_Descendent (Field8 (N));
         Visit_Descendent (Field9 (N));
         Visit_Descendent (Field10 (N));
         Visit_Descendent (Field11 (N));
         Visit_Descendent (Field12 (N));
         Visit_Descendent (Field13 (N));
         Visit_Descendent (Field14 (N));
         Visit_Descendent (Field15 (N));
         Visit_Descendent (Field16 (N));
         Visit_Descendent (Field17 (N));
         Visit_Descendent (Field18 (N));
         Visit_Descendent (Field19 (N));
         Visit_Descendent (Field20 (N));
         Visit_Descendent (Field21 (N));
         Visit_Descendent (Field22 (N));
         Visit_Descendent (Field23 (N));

         --  Now an interesting kludge. Normally parents are always printed
         --  since we traverse the tree in a downwards direction. There is
         --  however an exception to this rule, which is the case where a
         --  parent is constructed by the compiler and is not referenced
         --  elsewhere in the tree. The following catches this case

         if not Comes_From_Source (N) then
            Visit_Descendent (Union_Id (Parent (N)));
         end if;

         --  You may be wondering why we omitted Field2 above. The answer
         --  is that this is the Next_Entity field, and we want to treat
         --  it rather specially. Why? Because a Next_Entity link does not
         --  correspond to a level deeper in the tree, and we do not want
         --  the tree to march off to the right of the page due to bogus
         --  indentations coming from this effect.

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
                  Visit_Descendent (Union_Id (Next_Entity (Nod)));
                  Nod := Next_Entity (Nod);
               end loop;
            end;
         end if;
      end if;
   end Visit_Node;

end Treepr;
