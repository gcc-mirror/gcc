------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              S Y S T E M . T R A C E B A C K _ E N T R I E S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2003-2007, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  This package offers an abstraction of what is stored in traceback arrays
--  for call-chain computation purposes. By default, as defined in this
--  version of the package, an entry is a mere code location representing the
--  address of a call instruction part of the call-chain.

pragma Warnings (Off);
pragma Compiler_Unit;
pragma Warnings (On);

package System.Traceback_Entries is
   pragma Preelaborate;

   subtype Traceback_Entry is System.Address;
   --  This subtype defines what each traceback array entry contains

   Null_TB_Entry : constant Traceback_Entry := System.Null_Address;
   --  This is the value to be used when initializing an entry

   function PC_For (TB_Entry : Traceback_Entry) return System.Address;
   pragma Inline (PC_For);
   --  Returns the address of the call instruction associated with the
   --  provided entry.

   function TB_Entry_For (PC : System.Address) return Traceback_Entry;
   pragma Inline (TB_Entry_For);
   --  Returns an entry representing a frame for a call instruction at PC

end System.Traceback_Entries;
