------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . D W A R F _ L I N E S                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2009-2021, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Exceptions.Traceback; use Ada.Exceptions.Traceback;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Generic_Array_Sort;

with Interfaces; use Interfaces;

with System;                   use System;
with System.Storage_Elements;  use System.Storage_Elements;
with System.Address_Image;
with System.IO;                use System.IO;
with System.Object_Reader;     use System.Object_Reader;
with System.Traceback_Entries; use System.Traceback_Entries;
with System.Mmap;              use System.Mmap;
with System.Bounded_Strings;   use System.Bounded_Strings;

package body System.Dwarf_Lines is

   SSU : constant := System.Storage_Unit;

   function String_Length (Str : Str_Access) return Natural;
   --  Return the length of the C string Str

   ---------------------------------
   -- DWARF Parser Implementation --
   ---------------------------------

   procedure Read_Initial_Length
     (S    : in out Mapped_Stream;
      Len  :    out Offset;
      Is64 :    out Boolean);
   --  Read initial length as specified by Dwarf-4 7.2.2

   procedure Read_Section_Offset
     (S    : in out Mapped_Stream;
      Len  :    out Offset;
      Is64 :        Boolean);
   --  Read a section offset, as specified by Dwarf-4 7.4

   procedure Read_Aranges_Entry
     (C     : in out Dwarf_Context;
      Start :    out Storage_Offset;
      Len   :    out Storage_Count);
   --  Read a single .debug_aranges pair

   procedure Read_Aranges_Header
     (C           : in out Dwarf_Context;
      Info_Offset :    out Offset;
      Success     :    out Boolean);
   --  Read .debug_aranges header

   procedure Aranges_Lookup
     (C           : in out Dwarf_Context;
      Addr        :        Storage_Offset;
      Info_Offset :    out Offset;
      Success     :    out Boolean);
   --  Search for Addr in .debug_aranges and return offset Info_Offset in
   --  .debug_info.

   procedure Skip_Form
     (S      : in out Mapped_Stream;
      Form   :        uint32;
      Is64   :        Boolean;
      Ptr_Sz :        uint8);
   --  Advance offset in S for Form.

   procedure Seek_Abbrev
     (C             : in out Dwarf_Context;
      Abbrev_Offset :        Offset;
      Abbrev_Num    :        uint32);
   --  Seek to abbrev Abbrev_Num (starting from Abbrev_Offset)

   procedure Debug_Info_Lookup
     (C           : in out Dwarf_Context;
      Info_Offset :        Offset;
      Line_Offset :    out Offset;
      Success     :    out Boolean);
   --  Search for stmt_list tag in Info_Offset and set Line_Offset to the
   --  offset in .debug_lines. Only look at the first DIE, which should be
   --  a compilation unit.

   procedure Initialize_Pass (C : in out Dwarf_Context);
   --  Seek to the first byte of the first prologue and prepare to make a pass
   --  over the line number entries.

   procedure Initialize_State_Machine (C : in out Dwarf_Context);
   --  Set all state machine registers to their specified initial values

   procedure Parse_Prologue (C : in out Dwarf_Context);
   --  Decode a DWARF statement program prologue

   procedure Read_And_Execute_Isn
     (C    : in out Dwarf_Context;
      Done :    out Boolean);
   --  Read an execute a statement program instruction

   function To_File_Name
     (C    : in out Dwarf_Context;
      Code :        uint32) return String;
   --  Extract a file name from the prologue

   type Callback is not null access procedure (C : in out Dwarf_Context);
   procedure For_Each_Row (C : in out Dwarf_Context; F : Callback);
   --  Traverse each .debug_line entry with a callback

   procedure Dump_Row (C : in out Dwarf_Context);
   --  Dump a single row

   function "<" (Left, Right : Search_Entry) return Boolean;
   --  For sorting Search_Entry

   procedure Sort_Search_Array is new Ada.Containers.Generic_Array_Sort
     (Index_Type   => Natural,
      Element_Type => Search_Entry,
      Array_Type   => Search_Array);

   procedure Symbolic_Address
     (C           : in out Dwarf_Context;
      Addr        :        Storage_Offset;
      Dir_Name    :    out Str_Access;
      File_Name   :    out Str_Access;
      Subprg_Name :    out String_Ptr_Len;
      Line_Num    :    out Natural);
   --  Symbolize one address

   -----------------------
   --  DWARF constants  --
   -----------------------

   --  6.2.5.2 Standard Opcodes

   DW_LNS_copy               : constant := 1;
   DW_LNS_advance_pc         : constant := 2;
   DW_LNS_advance_line       : constant := 3;
   DW_LNS_set_file           : constant := 4;
   DW_LNS_set_column         : constant := 5;
   DW_LNS_negate_stmt        : constant := 6;
   DW_LNS_set_basic_block    : constant := 7;
   DW_LNS_const_add_pc       : constant := 8;
   DW_LNS_fixed_advance_pc   : constant := 9;
   DW_LNS_set_prologue_end   : constant := 10;
   DW_LNS_set_epilogue_begin : constant := 11;
   DW_LNS_set_isa            : constant := 12;

   --  6.2.5.3 Extended Opcodes

   DW_LNE_end_sequence : constant := 1;
   DW_LNE_set_address  : constant := 2;
   DW_LNE_define_file  : constant := 3;

   --  From the DWARF version 4 public review draft

   DW_LNE_set_discriminator : constant := 4;

   --  Attribute encodings

   DW_TAG_Compile_Unit : constant := 16#11#;

   DW_AT_Stmt_List : constant := 16#10#;

   DW_FORM_addr         : constant := 16#01#;
   DW_FORM_block2       : constant := 16#03#;
   DW_FORM_block4       : constant := 16#04#;
   DW_FORM_data2        : constant := 16#05#;
   DW_FORM_data4        : constant := 16#06#;
   DW_FORM_data8        : constant := 16#07#;
   DW_FORM_string       : constant := 16#08#;
   DW_FORM_block        : constant := 16#09#;
   DW_FORM_block1       : constant := 16#0a#;
   DW_FORM_data1        : constant := 16#0b#;
   DW_FORM_flag         : constant := 16#0c#;
   DW_FORM_sdata        : constant := 16#0d#;
   DW_FORM_strp         : constant := 16#0e#;
   DW_FORM_udata        : constant := 16#0f#;
   DW_FORM_ref_addr     : constant := 16#10#;
   DW_FORM_ref1         : constant := 16#11#;
   DW_FORM_ref2         : constant := 16#12#;
   DW_FORM_ref4         : constant := 16#13#;
   DW_FORM_ref8         : constant := 16#14#;
   DW_FORM_ref_udata    : constant := 16#15#;
   DW_FORM_indirect     : constant := 16#16#;
   DW_FORM_sec_offset   : constant := 16#17#;
   DW_FORM_exprloc      : constant := 16#18#;
   DW_FORM_flag_present : constant := 16#19#;
   DW_FORM_ref_sig8     : constant := 16#20#;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Search_Entry) return Boolean is
   begin
      return Left.First < Right.First;
   end "<";

   -----------
   -- Close --
   -----------

   procedure Close (C : in out Dwarf_Context) is
      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (Object_File,
         Object_File_Access);
      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (Search_Array,
         Search_Array_Access);
   begin
      if C.Has_Debug then
         Close (C.Lines);
         Close (C.Abbrev);
         Close (C.Info);
         Close (C.Aranges);
      end if;

      Close (C.Obj.all);
      Unchecked_Deallocation (C.Obj);

      Unchecked_Deallocation (C.Cache);
   end Close;

   ----------
   -- Dump --
   ----------

   procedure Dump (C : in out Dwarf_Context) is
   begin
      For_Each_Row (C, Dump_Row'Access);
   end Dump;

   --------------
   -- Dump_Row --
   --------------

   procedure Dump_Row (C : in out Dwarf_Context) is
      PC  : constant Integer_Address := Integer_Address (C.Registers.Address);
      Off : Offset;
   begin
      Tell (C.Lines, Off);

      Put (System.Address_Image (To_Address (PC)));
      Put (" ");
      Put (To_File_Name (C, C.Registers.File));
      Put (":");

      declare
         Image : constant String := uint32'Image (C.Registers.Line);
      begin
         Put_Line (Image (2 .. Image'Last));
      end;

      Seek (C.Lines, Off);
   end Dump_Row;

   procedure Dump_Cache (C : Dwarf_Context) is
      Cache : constant Search_Array_Access := C.Cache;
      S     : Object_Symbol;
      Name  : String_Ptr_Len;
   begin
      if Cache = null then
         Put_Line ("No cache");
         return;
      end if;
      for I in Cache'Range loop
         declare
            E : Search_Entry renames Cache (I);
            Base_Address : constant System.Address :=
              To_Address (Integer_Address (C.Low + Storage_Count (E.First)));
         begin
            Put (System.Address_Image (Base_Address));
            Put (" - ");
            Put (System.Address_Image (Base_Address + Storage_Count (E.Size)));
            Put (" l@");
            Put (System.Address_Image (To_Address (Integer_Address (E.Line))));
            Put (": ");
            S    := Read_Symbol (C.Obj.all, Offset (E.Sym));
            Name := Object_Reader.Name (C.Obj.all, S);
            Put (String (Name.Ptr (1 .. Name.Len)));
            New_Line;
         end;
      end loop;
   end Dump_Cache;

   ------------------
   -- For_Each_Row --
   ------------------

   procedure For_Each_Row (C : in out Dwarf_Context; F : Callback) is
      Done : Boolean;

   begin
      Initialize_Pass (C);

      loop
         Read_And_Execute_Isn (C, Done);

         if C.Registers.Is_Row then
            F.all (C);
         end if;

         exit when Done;
      end loop;
   end For_Each_Row;

   ---------------------
   -- Initialize_Pass --
   ---------------------

   procedure Initialize_Pass (C : in out Dwarf_Context) is
   begin
      Seek (C.Lines, 0);
      C.Next_Prologue := 0;

      Initialize_State_Machine (C);
   end Initialize_Pass;

   ------------------------------
   -- Initialize_State_Machine --
   ------------------------------

   procedure Initialize_State_Machine (C : in out Dwarf_Context) is
   begin
      C.Registers :=
        (Address        => 0,
         File           => 1,
         Line           => 1,
         Column         => 0,
         Is_Stmt        => C.Prologue.Default_Is_Stmt = 0,
         Basic_Block    => False,
         End_Sequence   => False,
         Prologue_End   => False,
         Epilogue_Begin => False,
         ISA            => 0,
         Is_Row         => False);
   end Initialize_State_Machine;

   ---------------
   -- Is_Inside --
   ---------------

   function Is_Inside (C : Dwarf_Context; Addr : Address) return Boolean is
   begin
      return (Addr >= C.Low + C.Load_Address
                and then Addr <= C.High + C.Load_Address);
   end Is_Inside;

   -----------------
   -- Low_Address --
   -----------------

   function Low_Address (C : Dwarf_Context)
      return System.Address is
   begin
      return C.Load_Address + C.Low;
   end Low_Address;

   ----------
   -- Open --
   ----------

   procedure Open
     (File_Name :     String;
      C         : out Dwarf_Context;
      Success   : out Boolean)
   is
      Line_Sec, Info_Sec, Abbrev_Sec, Aranges_Sec : Object_Section;
      Hi, Lo                                      : uint64;
   begin
      --  Not a success by default

      Success := False;

      --  Open file

      C.Obj := Open (File_Name, C.In_Exception);

      if C.Obj = null then
         return;
      end if;

      Success := True;

      --  Get memory bounds for executable code.  Note that such code
      --  might come from multiple sections.

      Get_Xcode_Bounds (C.Obj.all, Lo, Hi);
      C.Low  := Storage_Offset (Lo);
      C.High := Storage_Offset (Hi);

      --  Create a stream for debug sections

      if Format (C.Obj.all) = XCOFF32 then
         Line_Sec    := Get_Section (C.Obj.all, ".dwline");
         Abbrev_Sec  := Get_Section (C.Obj.all, ".dwabrev");
         Info_Sec    := Get_Section (C.Obj.all, ".dwinfo");
         Aranges_Sec := Get_Section (C.Obj.all, ".dwarnge");
      else
         Line_Sec    := Get_Section (C.Obj.all, ".debug_line");
         Abbrev_Sec  := Get_Section (C.Obj.all, ".debug_abbrev");
         Info_Sec    := Get_Section (C.Obj.all, ".debug_info");
         Aranges_Sec := Get_Section (C.Obj.all, ".debug_aranges");
      end if;

      if Line_Sec = Null_Section
        or else Abbrev_Sec = Null_Section
        or else Info_Sec = Null_Section
        or else Aranges_Sec = Null_Section
      then
         pragma Annotate
           (CodePeer, False_Positive,
            "test always true", "codepeer got confused");

         C.Has_Debug := False;
         return;
      end if;

      C.Lines   := Create_Stream (C.Obj.all, Line_Sec);
      C.Abbrev  := Create_Stream (C.Obj.all, Abbrev_Sec);
      C.Info    := Create_Stream (C.Obj.all, Info_Sec);
      C.Aranges := Create_Stream (C.Obj.all, Aranges_Sec);

      --  All operations are successful, context is valid

      C.Has_Debug := True;
   end Open;

   --------------------
   -- Parse_Prologue --
   --------------------

   procedure Parse_Prologue (C : in out Dwarf_Context) is
      Char : uint8;
      Prev : uint8;
      --  The most recently read character and the one preceding it

      Dummy : uint32;
      --  Destination for reads we don't care about

      Buf : Buffer;
      Off : Offset;

      First_Byte_Of_Prologue : Offset;
      Last_Byte_Of_Prologue  : Offset;

      Max_Op_Per_Insn : uint8;
      pragma Unreferenced (Max_Op_Per_Insn);

      Prologue : Line_Info_Prologue renames C.Prologue;

   begin
      Tell (C.Lines, First_Byte_Of_Prologue);
      Prologue.Unit_Length := Read (C.Lines);
      Tell (C.Lines, Off);
      C.Next_Prologue := Off + Offset (Prologue.Unit_Length);

      Prologue.Version         := Read (C.Lines);
      Prologue.Prologue_Length := Read (C.Lines);
      Tell (C.Lines, Last_Byte_Of_Prologue);
      Last_Byte_Of_Prologue :=
        Last_Byte_Of_Prologue + Offset (Prologue.Prologue_Length) - 1;

      Prologue.Min_Isn_Length := Read (C.Lines);

      if Prologue.Version >= 4 then
         Max_Op_Per_Insn := Read (C.Lines);
      end if;

      Prologue.Default_Is_Stmt := Read (C.Lines);
      Prologue.Line_Base       := Read (C.Lines);
      Prologue.Line_Range      := Read (C.Lines);
      Prologue.Opcode_Base     := Read (C.Lines);

      --  Opcode_Lengths is an array of Opcode_Base bytes specifying the number
      --  of LEB128 operands for each of the standard opcodes.

      for J in 1 .. uint32 (Prologue.Opcode_Base - 1) loop
         Prologue.Opcode_Lengths (J) := Read (C.Lines);
      end loop;

      --  The include directories table follows. This is a list of null
      --  terminated strings terminated by a double null. We only store
      --  its offset for later decoding.

      Tell (C.Lines, Prologue.Includes_Offset);
      Char := Read (C.Lines);

      if Char /= 0 then
         loop
            Prev := Char;
            Char := Read (C.Lines);
            exit when Char = 0 and Prev = 0;
         end loop;
      end if;

      --  The file_names table is next. Each record is a null terminated string
      --  for the file name, an unsigned LEB128 directory index, an unsigned
      --  LEB128 modification time, and an LEB128 file length. The table is
      --  terminated by a null byte.

      Tell (C.Lines, Prologue.File_Names_Offset);

      loop
         --  Read the filename

         Read_C_String (C.Lines, Buf);
         exit when Buf (0) = 0;
         Dummy := Read_LEB128 (C.Lines); --  Skip the directory index.
         Dummy := Read_LEB128 (C.Lines); --  Skip the modification time.
         Dummy := Read_LEB128 (C.Lines); --  Skip the file length.
      end loop;

      --  Check we're where we think we are. This sanity check ensures we think
      --  the prologue ends where the prologue says it does. It we aren't then
      --  we've probably gotten out of sync somewhere.

      Tell (C.Lines, Off);

      if Prologue.Unit_Length /= 0
        and then Off /= Last_Byte_Of_Prologue + 1
      then
         raise Dwarf_Error with "Parse error reading DWARF information";
      end if;
   end Parse_Prologue;

   --------------------------
   -- Read_And_Execute_Isn --
   --------------------------

   procedure Read_And_Execute_Isn
     (C    : in out Dwarf_Context;
      Done :    out Boolean)
   is
      Opcode          : uint8;
      Extended_Opcode : uint8;
      uint32_Operand  : uint32;
      int32_Operand   : int32;
      uint16_Operand  : uint16;
      Off             : Offset;

      Extended_Length : uint32;
      pragma Unreferenced (Extended_Length);

      Obj : Object_File renames C.Obj.all;
      Registers : Line_Info_Registers renames C.Registers;
      Prologue : Line_Info_Prologue renames C.Prologue;

   begin
      Done             := False;
      Registers.Is_Row := False;

      if Registers.End_Sequence then
         Initialize_State_Machine (C);
      end if;

      --  If we have reached the next prologue, read it. Beware of possibly
      --  empty blocks.

      --  When testing for the end of section, beware of possible zero padding
      --  at the end. Bail out as soon as there's not even room for at least a
      --  DW_LNE_end_sequence, 3 bytes from Off to Off+2. This resolves to
      --  Off+2 > Last_Offset_Within_Section, that is Off+2 > Section_Length-1,
      --  or Off+3 > Section_Length.

      Tell (C.Lines, Off);
      while Off = C.Next_Prologue loop
         Initialize_State_Machine (C);
         Parse_Prologue (C);
         Tell (C.Lines, Off);
         exit when Off + 3 > Length (C.Lines);
      end loop;

      --  Test whether we're done

      Tell (C.Lines, Off);

      --  We are finished when we either reach the end of the section, or we
      --  have reached zero padding at the end of the section.

      if Prologue.Unit_Length = 0 or else Off + 3 > Length (C.Lines) then
         Done := True;
         return;
      end if;

      --  Read and interpret an instruction

      Opcode := Read (C.Lines);

      --  Extended opcodes

      if Opcode = 0 then
         Extended_Length := Read_LEB128 (C.Lines);
         Extended_Opcode := Read (C.Lines);

         case Extended_Opcode is
            when DW_LNE_end_sequence =>

               --  Mark the end of a sequence of source locations

               Registers.End_Sequence := True;
               Registers.Is_Row       := True;

            when DW_LNE_set_address =>

               --  Set the program counter to a word

               Registers.Address := Read_Address (Obj, C.Lines);

            when DW_LNE_define_file =>

               --  Not implemented

               raise Dwarf_Error with "DWARF operator not implemented";

            when DW_LNE_set_discriminator =>

               --  Ignored

               int32_Operand := Read_LEB128 (C.Lines);

            when others =>

               --  Fail on an unrecognized opcode

               raise Dwarf_Error with "DWARF operator not implemented";
         end case;

      --  Standard opcodes

      elsif Opcode < Prologue.Opcode_Base then
         case Opcode is

            --  Append a row to the line info matrix

            when DW_LNS_copy =>
               Registers.Basic_Block := False;
               Registers.Is_Row      := True;

            --  Add an unsigned word to the program counter

            when DW_LNS_advance_pc =>
               uint32_Operand    := Read_LEB128 (C.Lines);
               Registers.Address :=
                 Registers.Address +
                 uint64 (uint32_Operand * uint32 (Prologue.Min_Isn_Length));

            --  Add a signed word to the current source line

            when DW_LNS_advance_line =>
               int32_Operand  := Read_LEB128 (C.Lines);
               Registers.Line :=
                 uint32 (int32 (Registers.Line) + int32_Operand);

            --  Set the current source file

            when DW_LNS_set_file =>
               uint32_Operand := Read_LEB128 (C.Lines);
               Registers.File := uint32_Operand;

            --  Set the current source column

            when DW_LNS_set_column =>
               uint32_Operand   := Read_LEB128 (C.Lines);
               Registers.Column := uint32_Operand;

            --  Toggle the "is statement" flag. GCC doesn't seem to set this???

            when DW_LNS_negate_stmt =>
               Registers.Is_Stmt := not Registers.Is_Stmt;

            --  Mark the beginning of a basic block

            when DW_LNS_set_basic_block =>
               Registers.Basic_Block := True;

            --  Advance the program counter as by the special opcode 255

            when DW_LNS_const_add_pc =>
               Registers.Address :=
                 Registers.Address +
                 uint64
                   (((255 - Prologue.Opcode_Base) / Prologue.Line_Range) *
                    Prologue.Min_Isn_Length);

            --  Advance the program counter by a constant

            when DW_LNS_fixed_advance_pc =>
               uint16_Operand    := Read (C.Lines);
               Registers.Address :=
                 Registers.Address + uint64 (uint16_Operand);

            --  The following are not implemented and ignored

            when DW_LNS_set_prologue_end =>
               null;

            when DW_LNS_set_epilogue_begin =>
               null;

            when DW_LNS_set_isa =>
               null;

            --  Anything else is an error

            when others =>
               raise Dwarf_Error with "DWARF operator not implemented";
         end case;

      --  Decode a special opcode. This is a line and address increment encoded
      --  in a single byte 'special opcode' as described in 6.2.5.1.

      else
         declare
            Address_Increment : int32;
            Line_Increment    : int32;

         begin
            Opcode := Opcode - Prologue.Opcode_Base;

            --  The adjusted opcode is a uint8 encoding an address increment
            --  and a signed line increment. The upperbound is allowed to be
            --  greater than int8'last so we decode using int32 directly to
            --  prevent overflows.

            Address_Increment :=
              int32 (Opcode / Prologue.Line_Range) *
              int32 (Prologue.Min_Isn_Length);
            Line_Increment :=
              int32 (Prologue.Line_Base) +
              int32 (Opcode mod Prologue.Line_Range);

            Registers.Address :=
              Registers.Address + uint64 (Address_Increment);
            Registers.Line := uint32 (int32 (Registers.Line) + Line_Increment);
            Registers.Basic_Block    := False;
            Registers.Prologue_End   := False;
            Registers.Epilogue_Begin := False;
            Registers.Is_Row         := True;
         end;
      end if;

   exception
      when Dwarf_Error =>

         --  In case of errors during parse, just stop reading

         Registers.Is_Row := False;
         Done             := True;
   end Read_And_Execute_Isn;

   ----------------------
   -- Set_Load_Address --
   ----------------------

   procedure Set_Load_Address (C : in out Dwarf_Context; Addr : Address) is
   begin
      C.Load_Address := Addr;
   end Set_Load_Address;

   ------------------
   -- To_File_Name --
   ------------------

   function To_File_Name
     (C    : in out Dwarf_Context;
      Code :        uint32) return String
   is
      Buf : Buffer;
      J   : uint32;

      Dir_Idx : uint32;
      pragma Unreferenced (Dir_Idx);

      Mod_Time : uint32;
      pragma Unreferenced (Mod_Time);

      Length : uint32;
      pragma Unreferenced (Length);

   begin
      Seek (C.Lines, C.Prologue.File_Names_Offset);

      --  Find the entry

      J := 0;
      loop
         J := J + 1;
         Read_C_String (C.Lines, Buf);

         if Buf (Buf'First) = 0 then
            return "???";
         end if;

         Dir_Idx  := Read_LEB128 (C.Lines);
         Mod_Time := Read_LEB128 (C.Lines);
         Length   := Read_LEB128 (C.Lines);
         exit when J = Code;
      end loop;

      return To_String (Buf);
   end To_File_Name;

   -------------------------
   -- Read_Initial_Length --
   -------------------------

   procedure Read_Initial_Length
     (S    : in out Mapped_Stream;
      Len  :    out Offset;
      Is64 :    out Boolean)
   is
      Len32 : uint32;
      Len64 : uint64;
   begin
      Len32 := Read (S);
      if Len32 < 16#ffff_fff0# then
         Is64 := False;
         Len  := Offset (Len32);
      elsif Len32 < 16#ffff_ffff# then
         --  Invalid length
         raise Constraint_Error;
      else
         Is64  := True;
         Len64 := Read (S);
         Len   := Offset (Len64);
      end if;
   end Read_Initial_Length;

   -------------------------
   -- Read_Section_Offset --
   -------------------------

   procedure Read_Section_Offset
     (S    : in out Mapped_Stream;
      Len  :    out Offset;
      Is64 :        Boolean)
   is
   begin
      if Is64 then
         Len := Offset (uint64'(Read (S)));
      else
         Len := Offset (uint32'(Read (S)));
      end if;
   end Read_Section_Offset;

   --------------------
   -- Aranges_Lookup --
   --------------------

   procedure Aranges_Lookup
     (C           : in out Dwarf_Context;
      Addr        :        Storage_Offset;
      Info_Offset :    out Offset;
      Success     :    out Boolean)
   is
   begin
      Info_Offset := 0;
      Seek (C.Aranges, 0);

      while Tell (C.Aranges) < Length (C.Aranges) loop
         Read_Aranges_Header (C, Info_Offset, Success);
         exit when not Success;

         loop
            declare
               Start : Storage_Offset;
               Len   : Storage_Count;
            begin
               Read_Aranges_Entry (C, Start, Len);
               exit when Start = 0 and Len = 0;
               if Addr >= Start
                 and then Addr < Start + Len
               then
                  Success := True;
                  return;
               end if;
            end;
         end loop;
      end loop;

      Success := False;
   end Aranges_Lookup;

   ---------------
   -- Skip_Form --
   ---------------

   procedure Skip_Form
     (S      : in out Mapped_Stream;
      Form   :        uint32;
      Is64   :        Boolean;
      Ptr_Sz :        uint8)
   is
      Skip : Offset;
   begin
      case Form is
         when DW_FORM_addr =>
            Skip := Offset (Ptr_Sz);
         when DW_FORM_block2 =>
            Skip := Offset (uint16'(Read (S)));
         when DW_FORM_block4 =>
            Skip := Offset (uint32'(Read (S)));
         when DW_FORM_data2 | DW_FORM_ref2 =>
            Skip := 2;
         when DW_FORM_data4 | DW_FORM_ref4 =>
            Skip := 4;
         when DW_FORM_data8 | DW_FORM_ref8 | DW_FORM_ref_sig8 =>
            Skip := 8;
         when DW_FORM_string =>
            while uint8'(Read (S)) /= 0 loop
               null;
            end loop;
            return;
         when DW_FORM_block | DW_FORM_exprloc =>
            Skip := Offset (uint32'(Read_LEB128 (S)));
         when DW_FORM_block1 | DW_FORM_ref1 =>
            Skip := Offset (uint8'(Read (S)));
         when DW_FORM_data1 | DW_FORM_flag =>
            Skip := 1;
         when DW_FORM_sdata =>
            declare
               Val : constant int32 := Read_LEB128 (S);
               pragma Unreferenced (Val);
            begin
               return;
            end;
         when DW_FORM_strp | DW_FORM_ref_addr | DW_FORM_sec_offset =>
            Skip := (if Is64 then 8 else 4);
         when DW_FORM_udata | DW_FORM_ref_udata =>
            declare
               Val : constant uint32 := Read_LEB128 (S);
               pragma Unreferenced (Val);
            begin
               return;
            end;
         when DW_FORM_flag_present =>
            return;
         when DW_FORM_indirect =>
            raise Constraint_Error;
         when others =>
            raise Constraint_Error;
      end case;
      Seek (S, Tell (S) + Skip);
   end Skip_Form;

   -----------------
   -- Seek_Abbrev --
   -----------------

   procedure Seek_Abbrev
     (C             : in out Dwarf_Context;
      Abbrev_Offset :        Offset;
      Abbrev_Num    :        uint32)
   is
      Num       : uint32;
      Abbrev    : uint32;
      Tag       : uint32;
      Has_Child : uint8;
      pragma Unreferenced (Abbrev, Tag, Has_Child);
   begin
      Seek (C.Abbrev, Abbrev_Offset);

      Num := 1;

      loop
         exit when Num = Abbrev_Num;

         Abbrev    := Read_LEB128 (C.Abbrev);
         Tag       := Read_LEB128 (C.Abbrev);
         Has_Child := Read (C.Abbrev);

         loop
            declare
               Name : constant uint32 := Read_LEB128 (C.Abbrev);
               Form : constant uint32 := Read_LEB128 (C.Abbrev);
            begin
               exit when Name = 0 and Form = 0;
            end;
         end loop;

         Num := Num + 1;
      end loop;
   end Seek_Abbrev;

   -----------------------
   -- Debug_Info_Lookup --
   -----------------------

   procedure Debug_Info_Lookup
     (C           : in out Dwarf_Context;
      Info_Offset :        Offset;
      Line_Offset :    out Offset;
      Success     :    out Boolean)
   is
      Unit_Length   : Offset;
      Is64          : Boolean;
      Version       : uint16;
      Abbrev_Offset : Offset;
      Addr_Sz       : uint8;
      Abbrev        : uint32;
      Has_Child     : uint8;
      pragma Unreferenced (Has_Child);
   begin
      Line_Offset := 0;
      Success := False;

      Seek (C.Info, Info_Offset);

      Read_Initial_Length (C.Info, Unit_Length, Is64);

      Version := Read (C.Info);
      if Version not in 2 .. 4 then
         return;
      end if;

      Read_Section_Offset (C.Info, Abbrev_Offset, Is64);

      Addr_Sz := Read (C.Info);
      if Addr_Sz /= (Address'Size / SSU) then
         return;
      end if;

      --  Read DIEs

      loop
         Abbrev := Read_LEB128 (C.Info);
         exit when Abbrev /= 0;
      end loop;

      --  Read abbrev table

      Seek_Abbrev (C, Abbrev_Offset, Abbrev);

      --  First ULEB128 is the abbrev code

      if Read_LEB128 (C.Abbrev) /= Abbrev then
         --  Ill formed abbrev table
         return;
      end if;

      --  Then the tag

      if Read_LEB128 (C.Abbrev) /= uint32'(DW_TAG_Compile_Unit) then
         --  Expect compile unit
         return;
      end if;

      --  Then the has child flag

      Has_Child := Read (C.Abbrev);

      loop
         declare
            Name : constant uint32 := Read_LEB128 (C.Abbrev);
            Form : constant uint32 := Read_LEB128 (C.Abbrev);
         begin
            exit when Name = 0 and Form = 0;
            if Name = DW_AT_Stmt_List then
               case Form is
                  when DW_FORM_sec_offset =>
                     Read_Section_Offset (C.Info, Line_Offset, Is64);
                  when DW_FORM_data4 =>
                     Line_Offset := Offset (uint32'(Read (C.Info)));
                  when DW_FORM_data8 =>
                     Line_Offset := Offset (uint64'(Read (C.Info)));
                  when others =>
                     --  Unhandled form
                     return;
               end case;

               Success := True;
               return;
            else
               Skip_Form (C.Info, Form, Is64, Addr_Sz);
            end if;
         end;
      end loop;

      return;
   end Debug_Info_Lookup;

   -------------------------
   -- Read_Aranges_Header --
   -------------------------

   procedure Read_Aranges_Header
     (C           : in out Dwarf_Context;
      Info_Offset :    out Offset;
      Success     :    out Boolean)
   is
      Unit_Length : Offset;
      Is64        : Boolean;
      Version     : uint16;
      Sz          : uint8;
   begin
      Success     := False;
      Info_Offset := 0;

      Read_Initial_Length (C.Aranges, Unit_Length, Is64);

      Version := Read (C.Aranges);
      if Version /= 2 then
         return;
      end if;

      Read_Section_Offset (C.Aranges, Info_Offset, Is64);

      --  Read address_size (ubyte)

      Sz := Read (C.Aranges);
      if Sz /= (Address'Size / SSU) then
         return;
      end if;

      --  Read segment_size (ubyte)

      Sz := Read (C.Aranges);
      if Sz /= 0 then
         return;
      end if;

      --  Handle alignment on twice the address size
      declare
         Cur_Off : constant Offset := Tell (C.Aranges);
         Align   : constant Offset := 2 * Address'Size / SSU;
         Space   : constant Offset := Cur_Off mod Align;
      begin
         if Space /= 0 then
            Seek (C.Aranges, Cur_Off + Align - Space);
         end if;
      end;

      Success := True;
   end Read_Aranges_Header;

   ------------------------
   -- Read_Aranges_Entry --
   ------------------------

   procedure Read_Aranges_Entry
     (C     : in out Dwarf_Context;
      Start :    out Storage_Offset;
      Len   :    out Storage_Count)
   is
   begin
      --  Read table
      if Address'Size = 32 then
         declare
            S, L : uint32;
         begin
            S     := Read (C.Aranges);
            L     := Read (C.Aranges);
            Start := Storage_Offset (S);
            Len   := Storage_Count (L);
         end;
      elsif Address'Size = 64 then
         declare
            S, L : uint64;
         begin
            S     := Read (C.Aranges);
            L     := Read (C.Aranges);
            Start := Storage_Offset (S);
            Len   := Storage_Count (L);
         end;
      else
         raise Constraint_Error;
      end if;
   end Read_Aranges_Entry;

   ------------------
   -- Enable_Cache --
   ------------------

   procedure Enable_Cache (C : in out Dwarf_Context) is
      Cache : Search_Array_Access;
   begin
      --  Phase 1: count number of symbols. Phase 2: fill the cache.
      declare
         S               : Object_Symbol;
         Val             : uint64;
         Xcode_Low       : constant uint64 := uint64 (C.Low);
         Xcode_High      : constant uint64 := uint64 (C.High);
         Sz              : uint32;
         Addr, Prev_Addr : uint32;
         Nbr_Symbols     : Natural;
      begin
         for Phase in 1 .. 2 loop
            Nbr_Symbols := 0;
            S           := First_Symbol (C.Obj.all);
            Prev_Addr   := uint32'Last;
            while S /= Null_Symbol loop
               --  Discard symbols of length 0 or located outside of the
               --  execution code section outer boundaries.
               Sz := uint32 (Size (S));
               Val := Value (S);

               if Sz > 0
                 and then Val >= Xcode_Low
                 and then Val <= Xcode_High
               then

                  Addr := uint32 (Val - Xcode_Low);

                  --  Try to filter symbols at the same address. This is a best
                  --  effort as they might not be consecutive.
                  if Addr /= Prev_Addr then
                     Nbr_Symbols := Nbr_Symbols + 1;
                     Prev_Addr   := Addr;

                     if Phase = 2 then
                        C.Cache (Nbr_Symbols) :=
                          (First => Addr,
                           Size  => Sz,
                           Sym   => uint32 (Off (S)),
                           Line  => 0);
                     end if;
                  end if;
               end if;

               S := Next_Symbol (C.Obj.all, S);
            end loop;

            if Phase = 1 then
               --  Allocate the cache
               Cache   := new Search_Array (1 .. Nbr_Symbols);
               C.Cache := Cache;
            end if;
         end loop;
         pragma Assert (Nbr_Symbols = C.Cache'Last);
      end;

      --  Sort the cache.
      Sort_Search_Array (C.Cache.all);

      --  Set line offsets
      if not C.Has_Debug then
         return;
      end if;
      declare
         Info_Offset : Offset;
         Line_Offset : Offset;
         Success     : Boolean;
         Ar_Start    : Storage_Offset;
         Ar_Len      : Storage_Count;
         Start, Len  : uint32;
         First, Last : Natural;
         Mid         : Natural;
      begin
         Seek (C.Aranges, 0);

         while Tell (C.Aranges) < Length (C.Aranges) loop
            Read_Aranges_Header (C, Info_Offset, Success);
            exit when not Success;

            Debug_Info_Lookup (C, Info_Offset, Line_Offset, Success);
            exit when not Success;

            --  Read table
            loop
               Read_Aranges_Entry (C, Ar_Start, Ar_Len);
               exit when Ar_Start = 0 and Ar_Len = 0;

               Len   := uint32 (Ar_Len);
               Start := uint32 (Ar_Start - C.Low);

               --  Search START in the array
               First := Cache'First;
               Last  := Cache'Last;
               Mid := First;  --  In case of array with one element
               while First < Last loop
                  Mid := First + (Last - First) / 2;
                  if Start < Cache (Mid).First then
                     Last := Mid - 1;
                  elsif Start >= Cache (Mid).First + Cache (Mid).Size then
                     First := Mid + 1;
                  else
                     exit;
                  end if;
               end loop;

               --  Fill info.

               --  There can be overlapping symbols
               while Mid > Cache'First
                 and then Cache (Mid - 1).First <= Start
                 and then Cache (Mid - 1).First + Cache (Mid - 1).Size > Start
               loop
                  Mid := Mid - 1;
               end loop;
               while Mid <= Cache'Last loop
                  if Start < Cache (Mid).First + Cache (Mid).Size
                    and then Start + Len > Cache (Mid).First
                  then
                     --  MID is within the bounds
                     Cache (Mid).Line := uint32 (Line_Offset);
                  elsif Start + Len <= Cache (Mid).First then
                     --  Over
                     exit;
                  end if;
                  Mid := Mid + 1;
               end loop;
            end loop;
         end loop;
      end;
   end Enable_Cache;

   ----------------------
   -- Symbolic_Address --
   ----------------------

   procedure Symbolic_Address
     (C           : in out Dwarf_Context;
      Addr        :        Storage_Offset;
      Dir_Name    :    out Str_Access;
      File_Name   :    out Str_Access;
      Subprg_Name :    out String_Ptr_Len;
      Line_Num    :    out Natural)
   is
      procedure Set_Result (Match : Line_Info_Registers);
      --  Set results using match

      procedure Set_Result (Match : Line_Info_Registers) is
         Dir_Idx : uint32;
         J       : uint32;

         Mod_Time : uint32;
         pragma Unreferenced (Mod_Time);

         Length : uint32;
         pragma Unreferenced (Length);

      begin
         Seek (C.Lines, C.Prologue.File_Names_Offset);

         --  Find the entry

         J := 0;
         loop
            J         := J + 1;
            File_Name := Read_C_String (C.Lines);

            if File_Name (File_Name'First) = ASCII.NUL then
               --  End of file list, so incorrect entry
               return;
            end if;

            Dir_Idx  := Read_LEB128 (C.Lines);
            Mod_Time := Read_LEB128 (C.Lines);
            Length   := Read_LEB128 (C.Lines);
            exit when J = Match.File;
         end loop;

         if Dir_Idx = 0 then
            --  No directory
            Dir_Name := null;

         else
            Seek (C.Lines, C.Prologue.Includes_Offset);

            J := 0;
            loop
               J        := J + 1;
               Dir_Name := Read_C_String (C.Lines);

               if Dir_Name (Dir_Name'First) = ASCII.NUL then
                  --  End of directory list, so ill-formed table
                  return;
               end if;

               exit when J = Dir_Idx;

            end loop;
         end if;

         Line_Num := Natural (Match.Line);
      end Set_Result;

      Addr_Int     : constant uint64 := uint64 (Addr);
      Previous_Row : Line_Info_Registers;
      Info_Offset  : Offset;
      Line_Offset  : Offset;
      Success      : Boolean;
      Done         : Boolean;
      S            : Object_Symbol;

   begin
      --  Initialize result
      Dir_Name    := null;
      File_Name   := null;
      Subprg_Name := (null, 0);
      Line_Num    := 0;

      if C.Cache /= null then
         --  Look in the cache
         declare
            Addr_Off         : constant uint32 := uint32 (Addr - C.Low);
            First, Last, Mid : Natural;
         begin
            First := C.Cache'First;
            Last  := C.Cache'Last;
            Mid   := First;

            while First <= Last loop
               Mid := First + (Last - First) / 2;
               if Addr_Off < C.Cache (Mid).First then
                  Last := Mid - 1;
               elsif Addr_Off >= C.Cache (Mid).First + C.Cache (Mid).Size then
                  First := Mid + 1;
               else
                  exit;
               end if;
            end loop;

            if Addr_Off >= C.Cache (Mid).First
              and then Addr_Off < C.Cache (Mid).First + C.Cache (Mid).Size
            then
               Line_Offset := Offset (C.Cache (Mid).Line);
               S := Read_Symbol (C.Obj.all, Offset (C.Cache (Mid).Sym));
               Subprg_Name := Object_Reader.Name (C.Obj.all, S);
            else
               --  Not found
               return;
            end if;
         end;
      else
         --  Search symbol
         S := First_Symbol (C.Obj.all);
         while S /= Null_Symbol loop
            if Spans (S, Addr_Int) then
               Subprg_Name := Object_Reader.Name (C.Obj.all, S);
               exit;
            end if;

            S := Next_Symbol (C.Obj.all, S);
         end loop;

         --  Search address in aranges table

         Aranges_Lookup (C, Addr, Info_Offset, Success);
         if not Success then
            return;
         end if;

         --  Search stmt_list in info table

         Debug_Info_Lookup (C, Info_Offset, Line_Offset, Success);
         if not Success then
            return;
         end if;
      end if;

      Seek (C.Lines, Line_Offset);
      C.Next_Prologue := 0;
      Initialize_State_Machine (C);
      Parse_Prologue (C);
      Previous_Row.Line := 0;

      --  Advance to the first entry

      loop
         Read_And_Execute_Isn (C, Done);

         if C.Registers.Is_Row then
            Previous_Row := C.Registers;
            exit;
         end if;

         exit when Done;
      end loop;

      --  Read the rest of the entries

      while Tell (C.Lines) < C.Next_Prologue loop
         Read_And_Execute_Isn (C, Done);

         if C.Registers.Is_Row then
            if not Previous_Row.End_Sequence
              and then Addr_Int >= Previous_Row.Address
              and then Addr_Int < C.Registers.Address
            then
               Set_Result (Previous_Row);
               return;

            elsif Addr_Int = C.Registers.Address then
               Set_Result (C.Registers);
               return;
            end if;

            Previous_Row := C.Registers;
         end if;

         exit when Done;
      end loop;
   end Symbolic_Address;

   -------------------
   -- String_Length --
   -------------------

   function String_Length (Str : Str_Access) return Natural is
   begin
      for I in Str'Range loop
         if Str (I) = ASCII.NUL then
            return I - Str'First;
         end if;
      end loop;
      return Str'Last;
   end String_Length;

   ------------------------
   -- Symbolic_Traceback --
   ------------------------

   procedure Symbolic_Traceback
     (Cin          :        Dwarf_Context;
      Traceback    :        AET.Tracebacks_Array;
      Suppress_Hex :        Boolean;
      Symbol_Found :    out Boolean;
      Res          : in out System.Bounded_Strings.Bounded_String)
   is
      use Ada.Characters.Handling;
      C : Dwarf_Context := Cin;

      Addr_In_Traceback : Address;
      Offset_To_Lookup  : Storage_Offset;

      Dir_Name    : Str_Access;
      File_Name   : Str_Access;
      Subprg_Name : String_Ptr_Len;
      Line_Num    : Natural;
      Off         : Natural;
   begin
      if not C.Has_Debug then
         Symbol_Found := False;
         return;
      else
         Symbol_Found := True;
      end if;

      for J in Traceback'Range loop
         --  If the buffer is full, no need to do any useless work
         exit when Is_Full (Res);

         Addr_In_Traceback := PC_For (Traceback (J));

         Offset_To_Lookup := Addr_In_Traceback - C.Load_Address;

         Symbolic_Address
           (C,
            Offset_To_Lookup,
            Dir_Name,
            File_Name,
            Subprg_Name,
            Line_Num);

         --  If we're not requested to suppress hex addresses, emit it now.

         if not Suppress_Hex then
            Append_Address (Res, Addr_In_Traceback);
            Append (Res, ' ');
         end if;

         if File_Name /= null then
            declare
               Last   : constant Natural := String_Length (File_Name);
               Is_Ada : constant Boolean :=
                 Last > 3
                 and then
                   To_Upper (String (File_Name (Last - 3 .. Last - 1))) =
                   ".AD";
               --  True if this is an Ada file. This doesn't take into account
               --  nonstandard file-naming conventions, but that's OK; this is
               --  purely cosmetic. It covers at least .ads, .adb, and .ada.

               Line_Image : constant String := Natural'Image (Line_Num);
            begin
               if Subprg_Name.Len /= 0 then
                  --  For Ada code, Symbol_Image is in all lower case; we don't
                  --  have the case from the original source code. But the best
                  --  guess is Mixed_Case, so convert to that.

                  if Is_Ada then
                     declare
                        Symbol_Image : String :=
                          Object_Reader.Decoded_Ada_Name
                            (C.Obj.all,
                             Subprg_Name);
                     begin
                        for K in Symbol_Image'Range loop
                           if K = Symbol_Image'First
                             or else not
                             (Is_Letter (Symbol_Image (K - 1))
                              or else Is_Digit (Symbol_Image (K - 1)))
                           then
                              Symbol_Image (K) := To_Upper (Symbol_Image (K));
                           end if;
                        end loop;
                        Append (Res, Symbol_Image);
                     end;
                  else
                     Off := Strip_Leading_Char (C.Obj.all, Subprg_Name);

                     Append
                       (Res,
                        String (Subprg_Name.Ptr (Off .. Subprg_Name.Len)));
                  end if;
               else
                  Append (Res, "???");
               end if;

               Append (Res, " at ");
               Append (Res, String (File_Name (1 .. Last)));
               Append (Res, ':');
               Append (Res, Line_Image (2 .. Line_Image'Last));
            end;
         else
            if Subprg_Name.Len > 0 then
               Off := Strip_Leading_Char (C.Obj.all, Subprg_Name);

               Append (Res, String (Subprg_Name.Ptr (Off .. Subprg_Name.Len)));
            else
               Append (Res, "???");
            end if;

            Append (Res, " at ???");
         end if;

         Append (Res, ASCII.LF);
      end loop;
   end Symbolic_Traceback;
end System.Dwarf_Lines;
