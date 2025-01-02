------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T R A C E B A C K                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2025, Free Software Foundation, Inc.         --
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

--  This package provides a method for generating a traceback of the current
--  execution location. The traceback shows the locations of calls in the call
--  chain, up to either the top or a designated number of levels.

with System.Traceback_Entries;

package System.Traceback is

   ----------------
   -- Call_Chain --
   ----------------

   procedure Call_Chain
     (Traceback   : in out System.Traceback_Entries.Tracebacks_Array;
      Max_Len     : Natural;
      Len         : out Natural;
      Exclude_Min : System.Address := System.Null_Address;
      Exclude_Max : System.Address := System.Null_Address;
      Skip_Frames : Natural := 1);
   --  Store up to Max_Len code locations in Traceback, corresponding to the
   --  current call chain.
   --
   --    Traceback is an array of addresses where the result will be stored.
   --
   --    Max_Len is the length of the Traceback array. If the call chain is
   --    longer than this, then additional entries are discarded, and the
   --    traceback is missing some of the highest level entries.
   --
   --    Len is the number of addresses returned in the Traceback array
   --
   --    Exclude_Min/Exclude_Max, if non null, provide a range of addresses
   --    to ignore from the computation of the traceback.
   --
   --    Skip_Frames says how many of the most recent calls should at least
   --    be excluded from the result, regardless of the exclusion bounds and
   --    starting with this procedure itself: 1 means exclude the frame for
   --    this procedure, 2 means 1 + exclude the frame for this procedure's
   --    caller, ...
   --
   --  On return, the Traceback array is filled in, and Len indicates the
   --  number of stored entries. The first entry is the most recent call,
   --  and the last entry is the highest level call.

   function C_Call_Chain
     (Traceback : System.Address;
      Max_Len   : Natural) return Natural;
   pragma Export (C, C_Call_Chain, "system__traceback__c_call_chain");
   --  Version that can be used directly from C

end System.Traceback;
