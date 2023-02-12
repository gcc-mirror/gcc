------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . D W A R F _ L I N E S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2009-2023, Free Software Foundation, Inc.        --
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

--  This package provides routines to read DWARF line number information from
--  a binary file with as little overhead as possible. This allows conversions
--  from PC addresses to human-readable source locations.
--
--  Files must be compiled with at least minimal debugging information (-g1).

with System.Bounded_Strings;
with System.Object_Reader;
with System.Traceback_Entries;

package System.Dwarf_Lines is

   package STE renames System.Traceback_Entries;
   package SOR renames System.Object_Reader;

   type Dwarf_Context (In_Exception : Boolean := False) is private;
   --  Type encapsulating the state of the DWARF reader. When In_Exception is
   --  True, we are parsing as part of an exception handler decorator so we do
   --  not want another exception to be raised and the parsing is done safely,
   --  skipping binary files that cannot be read or have been stripped from
   --  their debug sections for example.

   procedure Open
     (File_Name :     String;
      C         : out Dwarf_Context;
      Success   : out Boolean);
   procedure Close (C : in out Dwarf_Context);
   --  Open and close a file

   procedure Set_Load_Address (C : in out Dwarf_Context; Addr : Address);
   --  Set the run-time load address of a file. Used to rebase PIE (Position
   --  Independent Executable) binaries.

   function Is_Inside (C : Dwarf_Context; Addr : Address) return Boolean;
   pragma Inline (Is_Inside);
   --  Return whether a run-time address Addr lies within the file

   function Low_Address (C : Dwarf_Context) return Address;
   pragma Inline (Low_Address);
   --  Return the lowest run-time address of the file

   procedure Dump (C : in out Dwarf_Context);
   --  Dump each row found in the object's .debug_lines section to standard out

   procedure Dump_Cache (C : Dwarf_Context);
   --  Dump the cache (if present)

   procedure Enable_Cache (C : in out Dwarf_Context);
   --  Read symbol information to speed up Symbolic_Traceback.

   procedure Symbolic_Traceback
     (Cin          :        Dwarf_Context;
      Traceback    :        STE.Tracebacks_Array;
      Suppress_Hex :        Boolean;
      Symbol_Found :    out Boolean;
      Res          : in out System.Bounded_Strings.Bounded_String);
   --  Generate a string for a traceback suitable for displaying to the user.
   --  If one or more symbols are found, Symbol_Found is set to True. This
   --  allows the caller to fall back to hexadecimal addresses.

   Dwarf_Error : exception;
   --  Raised if a problem is encountered parsing DWARF information. Can be a
   --  result of a logic error or malformed DWARF information.

private
   --  The following section numbers reference

   --    "DWARF Debugging Information Format, Version 5"

   --  published by the Standards Group, http://freestandards.org.

   --  6.2.2 State Machine Registers

   type Line_Info_Registers is record
      Address            : SOR.uint64;
      File               : SOR.uint32;
      Line               : SOR.uint32;
      Column             : SOR.uint32;
      Is_Stmt            : Boolean;
      Basic_Block        : Boolean;
      End_Sequence       : Boolean;
      --  Prologue_End   : Boolean;
      --  Epilogue_Begin : Boolean;
      --  ISA            : SOR.uint32;
      --  Discriminator  : SOR.uint32;  -- DWARF 4/5
      Is_Row             : Boolean;     -- local
   end record;

   --  6.2.4 The Line Number Program Header

   MAX_OPCODE : constant := 256;

   type Opcode_Length_Array is array (1 .. MAX_OPCODE) of SOR.uint8;

   MAX_ENTRY : constant := 5;

   type Entry_Format_Pair is record
      C_Type : SOR.uint32;
      Form   : SOR.uint32;
   end record;

   type Entry_Format_Array is array (1 .. MAX_ENTRY) of Entry_Format_Pair;

   type Line_Info_Header is record
      Unit_Length                  : SOR.Offset;
      Version                      : SOR.uint16;
      Address_Size                 : SOR.uint8;           -- DWARF 5
      Segment_Selector_Size        : SOR.uint8;           -- DWARF 5
      Header_Length                : SOR.uint32;
      Minimum_Insn_Length          : SOR.uint8;
      Maximum_Op_Per_Insn          : SOR.uint8;           -- DWARF 4/5
      Default_Is_Stmt              : SOR.uint8;
      Line_Base                    : SOR.int8;
      Line_Range                   : SOR.uint8;
      Opcode_Base                  : SOR.uint8;
      --  Standard_Opcode_Lengths  : Opcode_Length_Array;
      Directory_Entry_Format_Count : SOR.uint8;           -- DWARF 5
      Directory_Entry_Format       : Entry_Format_Array;  -- DWARF 5
      Directories_Count            : SOR.uint32;          -- DWARF 5
      Directories                  : SOR.Offset;
      File_Name_Entry_Format_Count : SOR.uint8;           -- DWARF 5
      File_Name_Entry_Format       : Entry_Format_Array;  -- DWARF 5
      File_Names_Count             : SOR.uint32;          -- DWARF 5
      File_Names                   : SOR.Offset;
      Is64                         : Boolean;             -- local
   end record;

   type Search_Entry is record
      First : SOR.uint32;
      Size  : SOR.uint32;
      --  Function bounds as offset to the base address.

      Sym : SOR.uint32;
      --  Symbol offset to get the name.

      Line : SOR.uint32;
      --  Dwarf line offset.
   end record;

   type Search_Array is array (Natural range <>) of Search_Entry;

   type Search_Array_Access is access Search_Array;

   type Dwarf_Context (In_Exception : Boolean := False) is record
      Low, High : Address;
      --  Address bounds for executable code

      Obj : SOR.Object_File_Access;
      --  The object file containing dwarf sections

      Load_Address : Address := Null_Address;
      --  The address at which the object file was loaded at run time

      Has_Debug : Boolean;
      --  True if all debug sections are available

      Cache : Search_Array_Access;
      --  Quick access to symbol and debug info (when present).

      Abbrev   : SOR.Mapped_Stream;
      Aranges  : SOR.Mapped_Stream;
      Info     : SOR.Mapped_Stream;
      Lines    : SOR.Mapped_Stream;
      Line_Str : SOR.Mapped_Stream;  -- DWARF 5
      --  DWARF sections

      Header      : Line_Info_Header;
      Registers   : Line_Info_Registers;
      Next_Header : SOR.Offset;
      --  State for lines
   end record;

end System.Dwarf_Lines;
