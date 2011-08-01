------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             G N A T . T R A C E B A C K . S Y M B O L I C                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1999-2010, AdaCore                     --
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
