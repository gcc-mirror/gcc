------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S I N P U T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2022, Free Software Foundation, Inc.         --
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

pragma Style_Checks (All_Checks);
--  Subprograms not all in alpha order

with Atree;          use Atree;
with Debug;          use Debug;
with Opt;            use Opt;
with Output;         use Output;
with Scans;          use Scans;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Widechar;       use Widechar;

with GNAT.Byte_Order_Mark; use GNAT.Byte_Order_Mark;

with System.Storage_Elements;
with System.Memory;
with System.WCh_Con; use System.WCh_Con;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body Sinput is

   use ASCII, System;

   --  Routines to support conversion between types Lines_Table_Ptr,
   --  Logical_Lines_Table_Ptr and System.Address.

   pragma Warnings (Off);
   --  These unchecked conversions are aliasing safe, since they are never
   --  used to construct improperly aliased pointer values.

   function To_Address is
     new Ada.Unchecked_Conversion (Lines_Table_Ptr, Address);

   function To_Address is
     new Ada.Unchecked_Conversion (Logical_Lines_Table_Ptr, Address);

   function To_Pointer is
     new Ada.Unchecked_Conversion (Address, Lines_Table_Ptr);

   function To_Pointer is
     new Ada.Unchecked_Conversion (Address, Logical_Lines_Table_Ptr);

   pragma Warnings (On);

   -----------------------------
   -- Source_File_Index_Table --
   -----------------------------

   --  The Get_Source_File_Index function is called very frequently. Earlier
   --  versions cached a single entry, but then reverted to a serial search,
   --  and this proved to be a significant source of inefficiency. We then
   --  switched to using a table with a start point followed by a serial
   --  search. Now we make sure source buffers are on a reasonable boundary
   --  (see Types.Source_Align), and we can just use a direct look up in the
   --  following table.

   --  Note that this array is pretty large, but in most operating systems
   --  it will not be allocated in physical memory unless it is actually used.

   Source_File_Index_Table :
     array (Int range 0 .. 1 + (Int'Last / Source_Align)) of Source_File_Index;

   ---------------------------
   -- Add_Line_Tables_Entry --
   ---------------------------

   procedure Add_Line_Tables_Entry
     (S : in out Source_File_Record;
      P : Source_Ptr)
   is
      LL : Physical_Line_Number;

   begin
      --  Reallocate the lines tables if necessary

      --  Note: the reason we do not use the normal Table package
      --  mechanism is that we have several of these tables. We could
      --  use the new GNAT.Dynamic_Tables package and that would probably
      --  be a good idea ???

      if S.Last_Source_Line = S.Lines_Table_Max then
         Alloc_Line_Tables
           (S,
            Int (S.Last_Source_Line) *
              ((100 + Alloc.Lines_Increment) / 100));

         if Debug_Flag_D then
            Write_Str ("--> Reallocating lines table, size = ");
            Write_Int (Int (S.Lines_Table_Max));
            Write_Eol;
         end if;
      end if;

      S.Last_Source_Line := S.Last_Source_Line + 1;
      LL := S.Last_Source_Line;

      S.Lines_Table (LL) := P;

      --  Deal with setting new entry in logical lines table if one is
      --  present. Note that there is always space (because the call to
      --  Alloc_Line_Tables makes sure both tables are the same length),

      if S.Logical_Lines_Table /= null then

         --  We can always set the entry from the previous one, because
         --  the processing for a Source_Reference pragma ensures that
         --  at least one entry following the pragma is set up correctly.

         S.Logical_Lines_Table (LL) := S.Logical_Lines_Table (LL - 1) + 1;
      end if;
   end Add_Line_Tables_Entry;

   -----------------------
   -- Alloc_Line_Tables --
   -----------------------

   procedure Alloc_Line_Tables
     (S       : in out Source_File_Record;
      New_Max : Nat)
   is
      subtype size_t is Memory.size_t;

      New_Table : Lines_Table_Ptr;

      New_Logical_Table : Logical_Lines_Table_Ptr;

      New_Size : constant size_t :=
                   size_t (New_Max * Lines_Table_Type'Component_Size /
                                                             Storage_Unit);

   begin
      if S.Lines_Table = null then
         New_Table := To_Pointer (Memory.Alloc (New_Size));

      else
         New_Table :=
           To_Pointer (Memory.Realloc (To_Address (S.Lines_Table), New_Size));
      end if;

      if New_Table = null then
         raise Storage_Error;
      else
         S.Lines_Table     := New_Table;
         S.Lines_Table_Max := Physical_Line_Number (New_Max);
      end if;

      if S.Num_SRef_Pragmas /= 0 then
         if S.Logical_Lines_Table = null then
            New_Logical_Table := To_Pointer (Memory.Alloc (New_Size));
         else
            New_Logical_Table := To_Pointer
              (Memory.Realloc (To_Address (S.Logical_Lines_Table), New_Size));
         end if;

         if New_Logical_Table = null then
            raise Storage_Error;
         else
            S.Logical_Lines_Table := New_Logical_Table;
         end if;
      end if;
   end Alloc_Line_Tables;

   -----------------
   -- Backup_Line --
   -----------------

   procedure Backup_Line (P : in out Source_Ptr) is
      Sindex : constant Source_File_Index := Get_Source_File_Index (P);
      Src    : constant Source_Buffer_Ptr :=
                 Source_File.Table (Sindex).Source_Text;
      Sfirst : constant Source_Ptr :=
                 Source_File.Table (Sindex).Source_First;

   begin
      P := P - 1;

      if P = Sfirst then
         return;
      end if;

      if Src (P) = CR then
         if Src (P - 1) = LF then
            P := P - 1;
         end if;

      else -- Src (P) = LF
         if Src (P - 1) = CR then
            P := P - 1;
         end if;
      end if;

      --  Now find first character of the previous line

      while P > Sfirst
        and then Src (P - 1) /= LF
        and then Src (P - 1) /= CR
      loop
         P := P - 1;
      end loop;
   end Backup_Line;

   ---------------------------
   -- Build_Location_String --
   ---------------------------

   procedure Build_Location_String
     (Buf : in out Bounded_String;
      Loc : Source_Ptr)
   is
      Ptr : Source_Ptr := Loc;

   begin
      --  Loop through instantiations

      loop
         Append (Buf, Reference_Name (Get_Source_File_Index (Ptr)));
         Append (Buf, ':');
         Append (Buf, Nat (Get_Logical_Line_Number (Ptr)));

         Ptr := Instantiation_Location (Ptr);
         exit when Ptr = No_Location;
         Append (Buf, " instantiated at ");
      end loop;
   end Build_Location_String;

   function Build_Location_String (Loc : Source_Ptr) return String is
      Buf : Bounded_String;
   begin
      Build_Location_String (Buf, Loc);
      return +Buf;
   end Build_Location_String;

   -------------------
   -- Check_For_BOM --
   -------------------

   procedure Check_For_BOM is
      BOM : BOM_Kind;
      Len : Natural;
      Tst : String (1 .. 5);
      C   : Character;

   begin
      for J in 1 .. 5 loop
         C := Source (Scan_Ptr + Source_Ptr (J) - 1);

         --  Definitely no BOM if EOF character marks either end of file, or
         --  an illegal non-BOM character if not at the end of file.

         if C = EOF then
            return;
         end if;

         Tst (J) := C;
      end loop;

      Read_BOM (Tst, Len, BOM, XML_Support => False);

      case BOM is
         when UTF8_All =>
            Scan_Ptr := Scan_Ptr + Source_Ptr (Len);
            First_Non_Blank_Location := Scan_Ptr;
            Current_Line_Start := Scan_Ptr;
            Wide_Character_Encoding_Method := WCEM_UTF8;
            Upper_Half_Encoding := True;

         when UTF16_BE
            | UTF16_LE
         =>
            Set_Standard_Error;
            Write_Line ("UTF-16 encoding format not recognized");
            Set_Standard_Output;
            raise Unrecoverable_Error;

         when UTF32_BE
            | UTF32_LE
         =>
            Set_Standard_Error;
            Write_Line ("UTF-32 encoding format not recognized");
            Set_Standard_Output;
            raise Unrecoverable_Error;

         when Unknown =>
            null;

         when others =>
            raise Program_Error;
      end case;
   end Check_For_BOM;

   -----------------------------
   -- Clear_Source_File_Table --
   -----------------------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Lines_Table_Type, Lines_Table_Ptr);

   procedure Free is new Ada.Unchecked_Deallocation
     (Logical_Lines_Table_Type, Logical_Lines_Table_Ptr);

   procedure Clear_Source_File_Table is
   begin
      for X in 1 .. Source_File.Last loop
         declare
            S : Source_File_Record renames Source_File.Table (X);
         begin
            if S.Instance = No_Instance_Id then
               Free_Source_Buffer (S.Source_Text);
            else
               Free_Dope (S.Source_Text'Address);
               S.Source_Text := null;
            end if;

            Free (S.Lines_Table);
            Free (S.Logical_Lines_Table);
         end;
      end loop;

      Source_File.Free;
      Sinput.Initialize;
   end Clear_Source_File_Table;

   ---------------------------------
   -- Comes_From_Inherited_Pragma --
   ---------------------------------

   function Comes_From_Inherited_Pragma (S : Source_Ptr) return Boolean is
      SIE : Source_File_Record renames
              Source_File.Table (Get_Source_File_Index (S));
   begin
      return SIE.Inherited_Pragma;
   end Comes_From_Inherited_Pragma;

   -----------------------------
   -- Comes_From_Inlined_Body --
   -----------------------------

   function Comes_From_Inlined_Body (S : Source_Ptr) return Boolean is
      SIE : Source_File_Record renames
              Source_File.Table (Get_Source_File_Index (S));
   begin
      return SIE.Inlined_Body;
   end Comes_From_Inlined_Body;

   ------------------------
   -- Free_Source_Buffer --
   ------------------------

   procedure Free_Source_Buffer (Src : in out Source_Buffer_Ptr) is
      --  Unchecked_Deallocation doesn't work for access-to-constant; we need
      --  to first Unchecked_Convert to access-to-variable.

      function To_Source_Buffer_Ptr_Var is new
        Ada.Unchecked_Conversion (Source_Buffer_Ptr, Source_Buffer_Ptr_Var);

      Temp : Source_Buffer_Ptr_Var := To_Source_Buffer_Ptr_Var (Src);

      procedure Free_Ptr is new
        Ada.Unchecked_Deallocation (Source_Buffer, Source_Buffer_Ptr_Var);
   begin
      Free_Ptr (Temp);
      Src := null;
   end Free_Source_Buffer;

   -----------------------
   -- Get_Column_Number --
   -----------------------

   function Get_Column_Number (P : Source_Ptr) return Column_Number is
      S      : Source_Ptr;
      C      : Column_Number;
      Sindex : Source_File_Index;
      Src    : Source_Buffer_Ptr;

   begin
      --  If the input source pointer is not a meaningful value then return
      --  at once with column number 1. This can happen for a file not found
      --  condition for a file loaded indirectly by RTE, and also perhaps on
      --  some unknown internal error conditions. In either case we certainly
      --  don't want to blow up.

      if P < 1 then
         return 1;

      else
         Sindex := Get_Source_File_Index (P);
         Src := Source_File.Table (Sindex).Source_Text;
         S := Line_Start (P);
         C := 1;

         while S < P loop
            if Src (S) = HT then
               C := (C - 1) / 8 * 8 + (8 + 1);
               S := S + 1;

            --  Deal with wide character case, but don't include brackets
            --  notation in this circuit, since we know that this will
            --  display unencoded (no one encodes brackets notation).

            elsif Src (S) /= '[' and then Is_Start_Of_Wide_Char (Src, S) then
               C := C + 1;
               Skip_Wide (Src, S);

            --  Normal (non-wide) character case or brackets sequence

            else
               C := C + 1;
               S := S + 1;
            end if;
         end loop;

         return C;
      end if;
   end Get_Column_Number;

   -----------------------------
   -- Get_Logical_Line_Number --
   -----------------------------

   function Get_Logical_Line_Number
     (P : Source_Ptr) return Logical_Line_Number
   is
      SFR : Source_File_Record
              renames Source_File.Table (Get_Source_File_Index (P));

      L : constant Physical_Line_Number := Get_Physical_Line_Number (P);

   begin
      if SFR.Num_SRef_Pragmas = 0 then
         return Logical_Line_Number (L);
      else
         return SFR.Logical_Lines_Table (L);
      end if;
   end Get_Logical_Line_Number;

   ---------------------------------
   -- Get_Logical_Line_Number_Img --
   ---------------------------------

   function Get_Logical_Line_Number_Img
     (P : Source_Ptr) return String
   is
   begin
      Name_Len := 0;
      Add_Nat_To_Name_Buffer (Nat (Get_Logical_Line_Number (P)));
      return Name_Buffer (1 .. Name_Len);
   end Get_Logical_Line_Number_Img;

   ------------------------------
   -- Get_Physical_Line_Number --
   ------------------------------

   function Get_Physical_Line_Number
     (P : Source_Ptr) return Physical_Line_Number
   is
      Sfile : Source_File_Index;
      Table : Lines_Table_Ptr;
      Lo    : Physical_Line_Number;
      Hi    : Physical_Line_Number;
      Mid   : Physical_Line_Number;
      Loc   : Source_Ptr;

   begin
      --  If the input source pointer is not a meaningful value then return
      --  at once with line number 1. This can happen for a file not found
      --  condition for a file loaded indirectly by RTE, and also perhaps on
      --  some unknown internal error conditions. In either case we certainly
      --  don't want to blow up.

      if P < 1 then
         return 1;

      --  Otherwise we can do the binary search

      else
         Sfile := Get_Source_File_Index (P);
         Loc   := P + Source_File.Table (Sfile).Sloc_Adjust;
         Table := Source_File.Table (Sfile).Lines_Table;
         Lo    := 1;
         Hi    := Source_File.Table (Sfile).Last_Source_Line;

         loop
            Mid := (Lo + Hi) / 2;

            if Loc < Table (Mid) then
               Hi := Mid - 1;

            else -- Loc >= Table (Mid)

               if Mid = Hi or else
                  Loc < Table (Mid + 1)
               then
                  return Mid;
               else
                  Lo := Mid + 1;
               end if;

            end if;

         end loop;
      end if;
   end Get_Physical_Line_Number;

   ---------------------------
   -- Get_Source_File_Index --
   ---------------------------

   function Get_Source_File_Index (S : Source_Ptr) return Source_File_Index is
      Result : Source_File_Index;

      procedure Assertions;
      --  Assert various properties of the result

      procedure Assertions is

         --  ???The old version using zero-origin array indexing without array
         --  bounds checks returned 1 (i.e. system.ads) for these special
         --  locations, presumably by accident. We are mimicing that here.

         Special : constant Boolean :=
                     S = No_Location
                       or else S = Standard_Location
                       or else S = Standard_ASCII_Location
                       or else S = System_Location;

         pragma Assert ((S > No_Location) xor Special);
         pragma Assert (Result in Source_File.First .. Source_File.Last);

         SFR : Source_File_Record renames Source_File.Table (Result);

      begin
         --  SFR.Source_Text = null if and only if this is the SFR for a debug
         --  output file (*.dg), and that file is under construction. S can be
         --  slightly past Source_Last in that case because we haven't updated
         --  Source_Last.

         if Null_Source_Buffer_Ptr (SFR.Source_Text) then
            pragma Assert (S >= SFR.Source_First); null;
         else
            pragma Assert (SFR.Source_Text'First = SFR.Source_First);
            pragma Assert (SFR.Source_Text'Last = SFR.Source_Last);

            if not Special then
               pragma Assert (S in SFR.Source_First .. SFR.Source_Last);
               null;
            end if;
         end if;
      end Assertions;

   --  Start of processing for Get_Source_File_Index

   begin
      if S > No_Location then
         Result := Source_File_Index_Table (Int (S) / Source_Align);
      else
         Result := 1;
      end if;

      pragma Debug (Assertions);

      return Result;
   end Get_Source_File_Index;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Source_gnat_adc := No_Source_File;
      Source_File.Init;
      Instances.Init;
      Instances.Append (No_Location);
      pragma Assert (Instances.Last = No_Instance_Id);
   end Initialize;

   -------------------
   -- Instantiation --
   -------------------

   function Instantiation (S : SFI) return Source_Ptr is
      SIE : Source_File_Record renames Source_File.Table (S);
   begin
      if SIE.Inlined_Body or SIE.Inherited_Pragma then
         return SIE.Inlined_Call;
      else
         return Instances.Table (SIE.Instance);
      end if;
   end Instantiation;

   -------------------------
   -- Instantiation_Depth --
   -------------------------

   function Instantiation_Depth (S : Source_Ptr) return Nat is
      Sval  : Source_Ptr;
      Depth : Nat;

   begin
      Sval := S;
      Depth := 0;

      loop
         Sval := Instantiation_Location (Sval);
         exit when Sval = No_Location;
         Depth := Depth + 1;
      end loop;

      return Depth;
   end Instantiation_Depth;

   ----------------------------
   -- Instantiation_Location --
   ----------------------------

   function Instantiation_Location (S : Source_Ptr) return Source_Ptr is
   begin
      return Instantiation (Get_Source_File_Index (S));
   end Instantiation_Location;

   --------------------------
   -- Iterate_On_Instances --
   --------------------------

   procedure Iterate_On_Instances is
   begin
      for J in 1 .. Instances.Last loop
         Process (J, Instances.Table (J));
      end loop;
   end Iterate_On_Instances;

   ----------------------
   -- Last_Source_File --
   ----------------------

   function Last_Source_File return Source_File_Index is
   begin
      return Source_File.Last;
   end Last_Source_File;

   ----------------
   -- Line_Start --
   ----------------

   function Line_Start (P : Source_Ptr) return Source_Ptr is
      Sindex : constant Source_File_Index := Get_Source_File_Index (P);
      Src    : constant Source_Buffer_Ptr :=
                 Source_File.Table (Sindex).Source_Text;
      Sfirst : constant Source_Ptr :=
                 Source_File.Table (Sindex).Source_First;
      S      : Source_Ptr;

   begin
      S := P;
      while S > Sfirst
        and then Src (S - 1) /= CR
        and then Src (S - 1) /= LF
      loop
         S := S - 1;
      end loop;

      return S;
   end Line_Start;

   function Line_Start
     (L : Physical_Line_Number;
      S : Source_File_Index) return Source_Ptr
   is
   begin
      return Source_File.Table (S).Lines_Table (L);
   end Line_Start;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      Source_File.Release;
      Source_File.Locked := True;
   end Lock;

   ----------------------
   -- Num_Source_Files --
   ----------------------

   function Num_Source_Files return Nat is
   begin
      return Int (Source_File.Last) - Int (Source_File.First) + 1;
   end Num_Source_Files;

   ----------------------
   -- Num_Source_Lines --
   ----------------------

   function Num_Source_Lines (S : Source_File_Index) return Nat is
   begin
      return Nat (Source_File.Table (S).Last_Source_Line);
   end Num_Source_Lines;

   -----------------------
   -- Original_Location --
   -----------------------

   function Original_Location (S : Source_Ptr) return Source_Ptr is
      Sindex : Source_File_Index;
      Tindex : Source_File_Index;

   begin
      if S <= No_Location then
         return S;

      else
         Sindex := Get_Source_File_Index (S);

         if Instantiation (Sindex) = No_Location then
            return S;

         else
            Tindex := Template (Sindex);
            while Instantiation (Tindex) /= No_Location loop
               Tindex := Template (Tindex);
            end loop;

            return S - Source_First (Sindex) + Source_First (Tindex);
         end if;
      end if;
   end Original_Location;

   -------------------------
   -- Physical_To_Logical --
   -------------------------

   function Physical_To_Logical
     (Line : Physical_Line_Number;
      S    : Source_File_Index) return Logical_Line_Number
   is
      SFR : Source_File_Record renames Source_File.Table (S);

   begin
      if SFR.Num_SRef_Pragmas = 0 then
         return Logical_Line_Number (Line);
      else
         return SFR.Logical_Lines_Table (Line);
      end if;
   end Physical_To_Logical;

   --------------------------------
   -- Register_Source_Ref_Pragma --
   --------------------------------

   procedure Register_Source_Ref_Pragma
     (File_Name          : File_Name_Type;
      Stripped_File_Name : File_Name_Type;
      Mapped_Line        : Nat;
      Line_After_Pragma  : Physical_Line_Number)
   is
      subtype size_t is Memory.size_t;

      SFR : Source_File_Record renames Source_File.Table (Current_Source_File);

      ML : Logical_Line_Number;

   begin
      if File_Name /= No_File then
         SFR.Reference_Name := Stripped_File_Name;
         SFR.Full_Ref_Name  := File_Name;

         if not Debug_Generated_Code then
            SFR.Debug_Source_Name := Stripped_File_Name;
            SFR.Full_Debug_Name   := File_Name;
         end if;

         SFR.Num_SRef_Pragmas := SFR.Num_SRef_Pragmas + 1;
      end if;

      if SFR.Num_SRef_Pragmas = 1 then
         SFR.First_Mapped_Line := Logical_Line_Number (Mapped_Line);
      end if;

      if SFR.Logical_Lines_Table = null then
         SFR.Logical_Lines_Table := To_Pointer
           (Memory.Alloc
             (size_t (SFR.Lines_Table_Max *
                        Logical_Lines_Table_Type'Component_Size /
                                                        Storage_Unit)));
      end if;

      SFR.Logical_Lines_Table (Line_After_Pragma - 1) := No_Line_Number;

      ML := Logical_Line_Number (Mapped_Line);
      for J in Line_After_Pragma .. SFR.Last_Source_Line loop
         SFR.Logical_Lines_Table (J) := ML;
         ML := ML + 1;
      end loop;
   end Register_Source_Ref_Pragma;

   ---------------------------------
   -- Set_Source_File_Index_Table --
   ---------------------------------

   procedure Set_Source_File_Index_Table (Xnew : Source_File_Index) is
      Ind : Int;
      SP  : Source_Ptr;
      SL  : constant Source_Ptr := Source_File.Table (Xnew).Source_Last;
   begin
      SP  := Source_File.Table (Xnew).Source_First;
      pragma Assert (SP mod Source_Align = 0);
      Ind := Int (SP) / Source_Align;
      while SP <= SL loop
         Source_File_Index_Table (Ind) := Xnew;
         SP := SP + Source_Align;
         Ind := Ind + 1;
      end loop;
   end Set_Source_File_Index_Table;

   ---------------------------
   -- Skip_Line_Terminators --
   ---------------------------

   procedure Skip_Line_Terminators
     (P        : in out Source_Ptr;
      Physical : out Boolean)
   is
      Chr : constant Character := Source (P);

   begin
      if Chr = CR then
         if Source (P + 1) = LF then
            P := P + 2;
         else
            P := P + 1;
         end if;

      elsif Chr = LF then
         P := P + 1;

      elsif Chr = FF or else Chr = VT then
         P := P + 1;
         Physical := False;
         return;

         --  Otherwise we have a wide character

      else
         Skip_Wide (Source, P);
      end if;

      --  Fall through in the physical line terminator case. First deal with
      --  making a possible entry into the lines table if one is needed.

      --  Note: we are dealing with a real source file here, this cannot be
      --  the instantiation case, so we need not worry about Sloc adjustment.

      declare
         S : Source_File_Record
               renames Source_File.Table (Current_Source_File);

      begin
         Physical := True;

         --  Make entry in lines table if not already made (in some scan backup
         --  cases, we will be rescanning previously scanned source, so the
         --  entry may have already been made on the previous forward scan).

         if Source (P) /= EOF
           and then P > S.Lines_Table (S.Last_Source_Line)
         then
            Add_Line_Tables_Entry (S, P);
         end if;
      end;
   end Skip_Line_Terminators;

   --------------
   -- Set_Dope --
   --------------

   procedure Set_Dope
     (Src : System.Address; New_Dope : Dope_Ptr)
   is
      --  A fat pointer is a pair consisting of data pointer and dope pointer,
      --  in that order. So we want to overwrite the second word.
      Dope : System.Address;
      pragma Import (Ada, Dope);
      use System.Storage_Elements;
      for Dope'Address use Src + System.Address'Size / 8;
   begin
      Dope := New_Dope.all'Address;
   end Set_Dope;

   procedure Free_Dope (Src : System.Address) is
      Dope : Dope_Ptr;
      pragma Import (Ada, Dope);
      use System.Storage_Elements;
      for Dope'Address use Src + System.Address'Size / 8;
      procedure Free is new Ada.Unchecked_Deallocation (Dope_Rec, Dope_Ptr);
   begin
      Free (Dope);
   end Free_Dope;

   ----------------
   -- Sloc_Range --
   ----------------

   procedure Sloc_Range (N : Node_Id; Min, Max : out Source_Ptr) is

      Indx : constant Source_File_Index := Get_Source_File_Index (Sloc (N));

      function Process (N : Node_Id) return Traverse_Result;
      --  Process function for traversing the node tree

      procedure Traverse is new Traverse_Proc (Process);

      -------------
      -- Process --
      -------------

      function Process (N : Node_Id) return Traverse_Result is
         Loc : constant Source_Ptr := Sloc (Original_Node (N));

      begin
         --  Skip nodes that may have been added during expansion and
         --  that originate in other units, such as code for contracts
         --  in subprogram bodies.

         if Get_Source_File_Index (Loc) /= Indx then
            return Skip;
         end if;

         if Loc > No_Location then
            if Loc < Min then
               Min := Loc;
            elsif Loc > Max then
               Max := Loc;
            end if;
         end if;

         return OK_Orig;
      end Process;

   --  Start of processing for Sloc_Range

   begin
      Min := Sloc (N);
      Max := Min;
      Traverse (N);
   end Sloc_Range;

   -------------------
   -- Source_Offset --
   -------------------

   function Source_Offset (S : Source_Ptr) return Nat is
      Sindex : constant Source_File_Index := Get_Source_File_Index (S);
      Sfirst : constant Source_Ptr :=
                 Source_File.Table (Sindex).Source_First;
   begin
      return Nat (S - Sfirst);
   end Source_Offset;

   ------------------------
   -- Top_Level_Location --
   ------------------------

   function Top_Level_Location (S : Source_Ptr) return Source_Ptr is
      Oldloc : Source_Ptr;
      Newloc : Source_Ptr;

   begin
      Newloc := S;
      loop
         Oldloc := Newloc;
         Newloc := Instantiation_Location (Oldloc);
         exit when Newloc = No_Location;
      end loop;

      return Oldloc;
   end Top_Level_Location;

   --------------------
   -- Write_Location --
   --------------------

   procedure Write_Location (P : Source_Ptr) is
   begin
      if P = No_Location then
         Write_Str ("<no location>");

      elsif P <= Standard_Location then
         Write_Str ("<standard location>");

      else
         declare
            SI : constant Source_File_Index := Get_Source_File_Index (P);

         begin
            Write_Name_For_Debug (Debug_Source_Name (SI));
            Write_Char (':');
            Write_Int (Int (Get_Logical_Line_Number (P)));
            Write_Char (':');
            Write_Int (Int (Get_Column_Number (P)));

            if Instantiation (SI) /= No_Location then
               Write_Str (" [");
               Write_Location (Instantiation (SI));
               Write_Char (']');
            end if;
         end;
      end if;
   end Write_Location;

   ----------------------
   -- Write_Time_Stamp --
   ----------------------

   procedure Write_Time_Stamp (S : Source_File_Index) is
      T : constant Time_Stamp_Type := Time_Stamp (S);
      P : Natural;

   begin
      if T (1) = '9' then
         Write_Str ("19");
         P := 0;
      else
         Write_Char (T (1));
         Write_Char (T (2));
         P := 2;
      end if;

      Write_Char (T (P + 1));
      Write_Char (T (P + 2));
      Write_Char ('-');

      Write_Char (T (P + 3));
      Write_Char (T (P + 4));
      Write_Char ('-');

      Write_Char (T (P + 5));
      Write_Char (T (P + 6));
      Write_Char (' ');

      Write_Char (T (P + 7));
      Write_Char (T (P + 8));
      Write_Char (':');

      Write_Char (T (P + 9));
      Write_Char (T (P + 10));
      Write_Char (':');

      Write_Char (T (P + 11));
      Write_Char (T (P + 12));
   end Write_Time_Stamp;

   ----------------------------------------------
   -- Access Subprograms for Source File Table --
   ----------------------------------------------

   function Debug_Source_Name (S : SFI) return File_Name_Type is
   begin
      return Source_File.Table (S).Debug_Source_Name;
   end Debug_Source_Name;

   function Instance (S : SFI) return Instance_Id is
   begin
      return Source_File.Table (S).Instance;
   end Instance;

   function File_Name (S : SFI) return File_Name_Type is
   begin
      return Source_File.Table (S).File_Name;
   end File_Name;

   function File_Type (S : SFI) return Type_Of_File is
   begin
      return Source_File.Table (S).File_Type;
   end File_Type;

   function First_Mapped_Line (S : SFI) return Logical_Line_Number is
   begin
      return Source_File.Table (S).First_Mapped_Line;
   end First_Mapped_Line;

   function Full_Debug_Name (S : SFI) return File_Name_Type is
   begin
      return Source_File.Table (S).Full_Debug_Name;
   end Full_Debug_Name;

   function Full_File_Name (S : SFI) return File_Name_Type is
   begin
      return Source_File.Table (S).Full_File_Name;
   end Full_File_Name;

   function Full_Ref_Name (S : SFI) return File_Name_Type is
   begin
      return Source_File.Table (S).Full_Ref_Name;
   end Full_Ref_Name;

   function Identifier_Casing (S : SFI) return Casing_Type is
   begin
      return Source_File.Table (S).Identifier_Casing;
   end Identifier_Casing;

   function Inherited_Pragma (S : SFI) return Boolean is
   begin
      return Source_File.Table (S).Inherited_Pragma;
   end Inherited_Pragma;

   function Inlined_Body (S : SFI) return Boolean is
   begin
      return Source_File.Table (S).Inlined_Body;
   end Inlined_Body;

   function Inlined_Call (S : SFI) return Source_Ptr is
   begin
      return Source_File.Table (S).Inlined_Call;
   end Inlined_Call;

   function Keyword_Casing (S : SFI) return Casing_Type is
   begin
      return Source_File.Table (S).Keyword_Casing;
   end Keyword_Casing;

   function Last_Source_Line (S : SFI) return Physical_Line_Number is
   begin
      return Source_File.Table (S).Last_Source_Line;
   end Last_Source_Line;

   function License (S : SFI) return License_Type is
   begin
      return Source_File.Table (S).License;
   end License;

   function Num_SRef_Pragmas (S : SFI) return Nat is
   begin
      return Source_File.Table (S).Num_SRef_Pragmas;
   end Num_SRef_Pragmas;

   function Reference_Name (S : SFI) return File_Name_Type is
   begin
      return Source_File.Table (S).Reference_Name;
   end Reference_Name;

   function Source_Checksum (S : SFI) return Word is
   begin
      return Source_File.Table (S).Source_Checksum;
   end Source_Checksum;

   function Source_First (S : SFI) return Source_Ptr is
   begin
      return Source_File.Table (S).Source_First;
   end Source_First;

   function Source_Last (S : SFI) return Source_Ptr is
   begin
      return Source_File.Table (S).Source_Last;
   end Source_Last;

   function Source_Text (S : SFI) return Source_Buffer_Ptr is
   begin
      return Source_File.Table (S).Source_Text;
   end Source_Text;

   function Template (S : SFI) return SFI is
   begin
      return Source_File.Table (S).Template;
   end Template;

   function Time_Stamp (S : SFI) return Time_Stamp_Type is
   begin
      return Source_File.Table (S).Time_Stamp;
   end Time_Stamp;

   function Unit (S : SFI) return Unit_Number_Type is
   begin
      return Source_File.Table (S).Unit;
   end Unit;

   ------------------------------------------
   -- Set Procedures for Source File Table --
   ------------------------------------------

   procedure Set_Identifier_Casing (S : SFI; C : Casing_Type) is
   begin
      Source_File.Table (S).Identifier_Casing := C;
   end Set_Identifier_Casing;

   procedure Set_Keyword_Casing (S : SFI; C : Casing_Type) is
   begin
      Source_File.Table (S).Keyword_Casing := C;
   end Set_Keyword_Casing;

   procedure Set_License (S : SFI; L : License_Type) is
   begin
      Source_File.Table (S).License := L;
   end Set_License;

   procedure Set_Unit (S : SFI; U : Unit_Number_Type) is
   begin
      Source_File.Table (S).Unit := U;
   end Set_Unit;

   ----------------------
   -- Trim_Lines_Table --
   ----------------------

   procedure Trim_Lines_Table (S : Source_File_Index) is
      Max : constant Nat := Nat (Source_File.Table (S).Last_Source_Line);

   begin
      --  Release allocated storage that is no longer needed

      Source_File.Table (S).Lines_Table := To_Pointer
        (Memory.Realloc
          (To_Address (Source_File.Table (S).Lines_Table),
           Memory.size_t
            (Max * (Lines_Table_Type'Component_Size / System.Storage_Unit))));
      Source_File.Table (S).Lines_Table_Max := Physical_Line_Number (Max);
   end Trim_Lines_Table;

   ------------
   -- Unlock --
   ------------

   procedure Unlock is
   begin
      Source_File.Locked := False;
      Source_File.Release;
   end Unlock;

   --------
   -- wl --
   --------

   procedure wl (P : Source_Ptr) is
   begin
      Write_Location (P);
      Write_Eol;
   end wl;

end Sinput;
