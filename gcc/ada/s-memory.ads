------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         S Y S T E M . M E M O R Y                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--             Copyright (C) 2001 Free Software Foundation, Inc.            --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides the low level memory allocation/deallocation
--  mechanisms used by GNAT.

--  To provide an alternate implementation, simply recompile the modified
--  body of this package with gnatmake -u -a -g s-memory.adb and make sure
--  that the ali and object files for this unit are found in the object
--  search path.

package System.Memory is
   pragma Elaborate_Body;

   type size_t is mod 2 ** Standard'Address_Size;

   function Alloc (Size : size_t) return System.Address;
   --  malloc for use by GNAT, with error checking and task lockout,
   --  as well as allocation tracking.

   procedure Free (Ptr : System.Address);
   --  free for use by GNAT, with task lockout and allocation tracking.

   function Realloc
     (Ptr  : System.Address;
      Size : size_t)
      return System.Address;
   --  realloc for use by GNAT, with error checking and task lockout.

private

   pragma Export (C, Alloc, "__gnat_malloc");
   pragma Export (C, Free, "__gnat_free");
   pragma Export (C, Realloc, "__gnat_realloc");

end System.Memory;
