------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . C O M M U N I C A T I O N                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

--  Common support unit for GNAT.Sockets and GNAT.Serial_Communication

with Ada.Streams;
with System.CRTL;

package System.Communication is
   pragma Preelaborate;

   function Last_Index
     (First : Ada.Streams.Stream_Element_Offset;
      Count : CRTL.size_t) return Ada.Streams.Stream_Element_Offset;
   --  Compute the Last OUT parameter for the various Read / Receive
   --  subprograms: returns First + Count - 1.
   --
   --  When First = Stream_Element_Offset'First and Res = 0, Constraint_Error
   --  is raised. This is consistent with the semantics of stream operations
   --  as clarified in AI95-227.

end System.Communication;
