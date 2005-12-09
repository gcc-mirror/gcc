------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     I N T E R F A C E S . O S 2 L I B                    --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1993-2005, Free Software Foundation, Inc.         --
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
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package (and children) provide interface definitions to the standard
--  OS/2 Library. They are merely a translation of the various <bse*.h> files.

--  It is intended that higher level interfaces (with better names, and
--  stronger typing!) be built on top of this one for Ada (i.e. clean)
--  programming.

--  We have chosen to keep names, types, etc.  as close as possible to the
--  C definition to provide easier reference to the documentation. The main
--  exception is when a formal and its type (in C) differed only by the case
--  of letters (like in HMUX hmux). In this case, we have prepended "F_" to
--  the formal (i.e. F_hmux : HMUX).

with Interfaces.C;
with Interfaces.C.Strings;
with System;

package Interfaces.OS2Lib is
   pragma Preelaborate;

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;

   -------------------
   -- General Types --
   -------------------

   type    APIRET   is new IC.unsigned_long;
   type    APIRET16 is new IC.unsigned_short;
   subtype APIRET32 is     APIRET;

   subtype PSZ    is ICS.chars_ptr;
   subtype PCHAR  is ICS.chars_ptr;
   subtype PVOID  is System.Address;
   type    PPVOID is access all PVOID;

   type BOOL32 is new IC.unsigned_long;
   False32 : constant BOOL32 := 0;
   True32  : constant BOOL32 := 1;

   type UCHAR   is new IC.unsigned_char;
   type USHORT  is new IC.unsigned_short;
   type ULONG   is new IC.unsigned_long;
   type PULONG  is access all ULONG;

   --  Coprocessor stack register element

   type FPREG is record
      losig             : ULONG;        --  Low 32-bits of the mantissa
      hisig             : ULONG;        --  High 32-bits of the mantissa
      signexp           : USHORT;       --  Sign and exponent
   end record;
   pragma Convention (C, FPREG);

   type AULONG is array (IC.size_t range <>) of ULONG;
   type AFPREG is array (IC.size_t range <>) of FPREG;

   type LHANDLE is new IC.unsigned_long;

   NULLHANDLE : constant := 0;

   ---------------------
   -- Time Management --
   ---------------------

   function DosSleep (How_long : ULONG) return APIRET;
   pragma Import (C, DosSleep, "DosSleep");

   type DATETIME is record
      hours      : UCHAR;
      minutes    : UCHAR;
      seconds    : UCHAR;
      hundredths : UCHAR;
      day        : UCHAR;
      month      : UCHAR;
      year       : USHORT;
      timezone   : IC.short;
      weekday    : UCHAR;
   end record;

   type PDATETIME is access all DATETIME;

   function DosGetDateTime (pdt : PDATETIME) return APIRET;
   pragma Import (C, DosGetDateTime, "DosGetDateTime");

   function DosSetDateTime (pdt : PDATETIME) return APIRET;
   pragma Import (C, DosSetDateTime, "DosSetDateTime");

   ----------------------------
   -- Miscelleneous Features --
   ----------------------------

   --  Features which do not fit any child

   function DosBeep (Freq : ULONG; Dur : ULONG) return APIRET;
   pragma Import (C, DosBeep, "DosBeep");

   procedure Must_Not_Fail (Return_Code : OS2Lib.APIRET);
   pragma Inline (Must_Not_Fail);
   --  Many OS/2 functions return APIRET and are not supposed to fail. In C
   --  style, these would be called as procedures, disregarding the returned
   --  value. This procedure can be used to achieve the same effect with a
   --  call of the form: Must_Not_Fail (Some_OS2_Function (...));

   procedure Sem_Must_Not_Fail (Return_Code : OS2Lib.APIRET);
   pragma Inline (Sem_Must_Not_Fail);
   --  Similar to Must_Not_Fail, but used in the case of DosPostEventSem,
   --  where the "error" code ERROR_ALREADY_POSTED is not really an error.

end Interfaces.OS2Lib;
