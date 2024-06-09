------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   A D A . C A L E N D A R . D E L A Y S                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This package implements Calendar.Time delays using protected objects

--  Note: the compiler generates direct calls to this interface, in the
--  processing of time types.

package Ada.Calendar.Delays is

   procedure Delay_For (D : Duration);
   --  Delay until an interval of length (at least) D seconds has passed, or
   --  the task is aborted to at least the current ATC nesting level. This is
   --  an abort completion point. The body of this procedure must perform all
   --  the processing required for an abort point.

   procedure Delay_Until (T : Time);
   --  Delay until Clock has reached (at least) time T, or the task is aborted
   --  to at least the current ATC nesting level. The body of this procedure
   --  must perform all the processing required for an abort point.

   function To_Duration (T : Time) return Duration;
   --  Convert Time to Duration elapsed since UNIX epoch

end Ada.Calendar.Delays;
