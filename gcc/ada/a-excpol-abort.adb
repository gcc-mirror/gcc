------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   A D A . E X C E P T I O N S . P O L L                  --
--                (version supporting asynchronous abort test)              --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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

--  This version is for targets that do not support per-thread asynchronous
--  signals. On such targets, we require compilation with the -gnatP switch
--  that activates periodic polling. Then in the body of the polling routine
--  we test for asynchronous abort.

--  Windows, HPUX 10 and VMS currently use this file

pragma Warnings (Off);
--  Allow withing of non-Preelaborated units in Ada 2005 mode where this
--  package will be categorized as Preelaborate. See AI-362 for details.
--  It is safe in the context of the run-time to violate the rules.

with System.Soft_Links;

pragma Warnings (On);

separate (Ada.Exceptions)

----------
-- Poll --
----------

procedure Poll is
begin
   --  Test for asynchronous abort on each poll

   if System.Soft_Links.Check_Abort_Status.all /= 0 then
      raise Standard'Abort_Signal;
   end if;
end Poll;
