------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             G N A T . T R A C E B A C K . S Y M B O L I C                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--           Copyright (C) 1999-2002 Ada Core Technologies, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

--  Run-time symbolic traceback support

with System.Soft_Links;
with Ada.Exceptions.Traceback; use Ada.Exceptions.Traceback;

package body GNAT.Traceback.Symbolic is

   pragma Linker_Options ("-laddr2line");
   pragma Linker_Options ("-lbfd");
   pragma Linker_Options ("-liberty");

   package TSL renames System.Soft_Links;

   ------------------------
   -- Symbolic_Traceback --
   ------------------------

   function Symbolic_Traceback (Traceback : Tracebacks_Array) return String is
      procedure convert_addresses
        (addrs    : System.Address;
         n_addr   : Integer;
         buf      : System.Address;
         len      : System.Address);
      pragma Import (C, convert_addresses, "convert_addresses");
      --  This is the procedure version of the Ada aware addr2line that will
      --  use argv[0] as the executable containing the debug information.
      --  This procedure is provided by libaddr2line on targets that support
      --  it. A dummy version is in a-adaint.c for other targets so that build
      --  of shared libraries doesn't generate unresolved symbols.
      --
      --  Note that this procedure is *not* thread-safe.

      Res : String (1 .. 256 * Traceback'Length);
      Len : Integer;

   begin
      if Traceback'Length > 0 then
         TSL.Lock_Task.all;
         convert_addresses
           (Traceback'Address, Traceback'Length, Res (1)'Address, Len'Address);
         TSL.Unlock_Task.all;
         return Res (1 .. Len);
      else
         return "";
      end if;
   end Symbolic_Traceback;

   function Symbolic_Traceback (E : Exception_Occurrence) return String is
   begin
      return Symbolic_Traceback (Tracebacks (E));
   end Symbolic_Traceback;

end GNAT.Traceback.Symbolic;
