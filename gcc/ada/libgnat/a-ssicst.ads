------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--      A D A . S T R E A M S . S T R E A M _ I O . C _ S T R E A M S       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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

--  This package provides an interface between Ada.Stream_IO and the
--  C streams. This allows sharing of a stream between Ada and C or C++,
--  as well as allowing the Ada program to operate directly on the stream.

with Interfaces.C_Streams;

package Ada.Streams.Stream_IO.C_Streams is

   package ICS renames Interfaces.C_Streams;

   function C_Stream (F : File_Type) return ICS.FILEs;
   --  Obtain stream from existing open file

   procedure Open
     (File     : in out File_Type;
      Mode     : File_Mode;
      C_Stream : ICS.FILEs;
      Form     : String := "";
      Name     : String := "");
   --  Create new file from existing stream

end Ada.Streams.Stream_IO.C_Streams;
