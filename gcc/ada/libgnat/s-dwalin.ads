------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . D W A R F _ L I N E S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2009-2018, Free Software Foundation, Inc.        --
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
--  a generic object file with as little overhead as possible. This allows
--  conversions from PC addresses to human readable source locations.
--
--  Objects must be built with debugging information, however only the
--  .debug_line section of the object file is referenced. In cases where object
--  size is a consideration it's possible to strip all other .debug sections,
--  which will decrease the size of the object significantly.

pragma Polling (Off);
--  We must turn polling off for this unit, because otherwise we can get
--  elaboration circularities when polling is turned on

with Ada.Exceptions.Traceback;

with System.Object_Reader;
with System.Storage_Elements;
with System.Bounded_Strings;

package System.Dwarf_Lines is

   package AET renames Ada.Exceptions.Traceback;
   package SOR renames System.Object_Reader;

   type Dwarf_Context (In_Exception : Boolean := False) is private;
   --  Type encapsulation the state of the Dwarf reader. When In_Exception
   --  is True we are parsing as part of a exception handler decorator, we do
   --  not want an exception to be raised, the parsing is done safely skipping
   --  DWARF file that cannot be read or with stripped debug section for
   --  example.

   procedure Open
     (File_Name :     String;
      C         : out Dwarf_Context;
      Success   : out Boolean);
   procedure Close (C : in out Dwarf_Context);
   --  Open and close files

   procedure Set_Load_Address (C : in out Dwarf_Context; Addr : Address);
   --  Set the load address of a file. This is used to rebase PIE (Position
   --  Independant Executable) binaries.

   function Is_Inside (C : Dwarf_Context; Addr : Address) return Boolean;
   pragma Inline (Is_Inside);
   --  Return true iff a run-time address Addr is within the module

   function Low_Address (C : Dwarf_Context)
      return System.Address;
   pragma Inline (Low_Address);
   --  Return the lowest address of C, accounting for the module load address

   procedure Dump (C : in out Dwarf_Context);
   --  Dump each row found in the object's .debug_lines section to standard out

   procedure Dump_Cache (C : Dwarf_Context);
   --  Dump the cache (if present)

   procedure Enable_Cache (C : in out Dwarf_Context);
   --  Read symbols information to speed up Symbolic_Traceback.

   procedure Symbolic_Traceback
     (Cin          :        Dwarf_Context;
      Traceback    :        AET.Tracebacks_Array;
      Suppress_Hex :        Boolean;
      Symbol_Found : in out Boolean;
      Res          : in out System.Bounded_Strings.Bounded_String);
   --  Generate a string for a traceback suitable for displaying to the user.
   --  If one or more symbols are found, Symbol_Found is set to True. This
   --  allows the caller to fall back to hexadecimal addresses.

   Dwarf_Error : exception;
   --  Raised if a problem is encountered parsing DWARF information. Can be a
   --  result of a logic error or malformed DWARF information.

private
   --  The following section numbers reference

   --    "DWARF Debugging Information Format, Version 3"

   --  published by the Standards Group, http://freestandards.org.

   --  6.2.2 State Machine Registers

   type Line_Info_Registers is record
      Address        : SOR.uint64;
      File           : SOR.uint32;
      Line           : SOR.uint32;
      Column         : SOR.uint32;
      Is_Stmt        : Boolean;
      Basic_Block    : Boolean;
      End_Sequence   : Boolean;
      Prologue_End   : Boolean;
      Epilogue_Begin : Boolean;
      ISA            : SOR.uint32;
      Is_Row         : Boolean;
   end record;

   --  6.2.4 The Line Number Program Prologue

   MAX_OPCODE_LENGTHS : constant := 256;

   type Opcodes_Lengths_Array is
     array (SOR.uint32 range 1 .. MAX_OPCODE_LENGTHS) of SOR.uint8;

   type Line_Info_Prologue is record
      Unit_Length       : SOR.uint32;
      Version           : SOR.uint16;
      Prologue_Length   : SOR.uint32;
      Min_Isn_Length    : SOR.uint8;
      Default_Is_Stmt   : SOR.uint8;
      Line_Base         : SOR.int8;
      Line_Range        : SOR.uint8;
      Opcode_Base       : SOR.uint8;
      Opcode_Lengths    : Opcodes_Lengths_Array;
      Includes_Offset   : SOR.Offset;
      File_Names_Offset : SOR.Offset;
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
      Low, High  : System.Storage_Elements.Storage_Offset;
      --  Bounds of the module, per the module object file

      Obj : SOR.Object_File_Access;
      --  The object file containing dwarf sections

      Load_Address : System.Address := System.Null_Address;
      --  The address at which the object file was loaded at run time

      Has_Debug : Boolean;
      --  True if all debug sections are available

      Cache : Search_Array_Access;
      --  Quick access to symbol and debug info (when present).

      Lines   : SOR.Mapped_Stream;
      Aranges : SOR.Mapped_Stream;
      Info    : SOR.Mapped_Stream;
      Abbrev  : SOR.Mapped_Stream;
      --  Dwarf line, aranges, info and abbrev sections

      Prologue      : Line_Info_Prologue;
      Registers     : Line_Info_Registers;
      Next_Prologue : SOR.Offset;
      --  State for lines
   end record;

end System.Dwarf_Lines;
