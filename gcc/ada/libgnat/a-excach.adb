------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--             A D A . E X C E P T I O N S . C A L L _ C H A I N            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

pragma Warnings (Off);
--  Allow withing of non-Preelaborated units in Ada 2005 mode where this
--  package will be categorized as Preelaborate. See AI-362 for details.
--  It is safe in the context of the run-time to violate the rules.

with System.Traceback;

pragma Warnings (On);

separate (Ada.Exceptions)
procedure Call_Chain (Excep : EOA) is

   Exception_Tracebacks : Integer;
   pragma Import (C, Exception_Tracebacks, "__gl_exception_tracebacks");
   --  Boolean indicating whether tracebacks should be stored in exception
   --  occurrences.

begin
   if Exception_Tracebacks /= 0 and Excep.Num_Tracebacks = 0 then

      --  If Exception_Tracebacks = 0 then the program was not
      --  compiled for storing tracebacks in exception occurrences
      --  (-bargs -E switch) so that we do not generate them.
      --
      --  If Excep.Num_Tracebacks /= 0 then this is a reraise, no need
      --  to store a new (wrong) chain.

      --  We ask System.Traceback.Call_Chain to skip 3 frames to ensure that
      --  itself, ourselves and our caller are not part of the result. Our
      --  caller is always an exception propagation actor that we don't want
      --  to see, and it may be part of a separate subunit which pulls it
      --  outside the AAA/ZZZ range.

      System.Traceback.Call_Chain
        (Traceback   => Excep.Tracebacks,
         Max_Len     => Max_Tracebacks,
         Len         => Excep.Num_Tracebacks,
         Exclude_Min => Code_Address_For_AAA,
         Exclude_Max => Code_Address_For_ZZZ,
         Skip_Frames => 3);
   end if;

end Call_Chain;
