------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             G N A T . T R A C E B A C K . S Y M B O L I C                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1999-2008, AdaCore                     --
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

--  Version used on unimplemented targets

--  Run-time symbolic traceback is currently supported on the following
--  targets:

--     HP-UX
--     IRIX
--     GNU/Linux x86
--     AIX
--     Solaris sparc
--     Tru64
--     OpenVMS/Alpha
--     Windows NT/XP/Vista

--  This version is used on all other targets, it generates a warning at
--  compile time if it is with'ed, and the bodies generate messages saying
--  that the functions are not implemented.

package body GNAT.Traceback.Symbolic is

   ------------------------
   -- Symbolic_Traceback --
   ------------------------

   function Symbolic_Traceback (Traceback : Tracebacks_Array) return String
   is
      pragma Unreferenced (Traceback);
   begin
      return "Symbolic_Traceback not implemented on this target";
   end Symbolic_Traceback;

   function Symbolic_Traceback (E : Exception_Occurrence) return String
   is
      pragma Unreferenced (E);
   begin
      return "Symbolic_Traceback not implemented on this target";
   end Symbolic_Traceback;

end GNAT.Traceback.Symbolic;
