------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--           A D A . S T R E A M S . S T O R A G E . B O U N D E D          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2020-2021, Free Software Foundation, Inc.      --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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
------------------------------------------------------------------------------

package Ada.Streams.Storage.Bounded with Pure is

   type Stream_Type (Max_Elements : Stream_Element_Count) is
     new Storage_Stream_Type with private with
      Default_Initial_Condition => Element_Count (Stream_Type) = 0;

   overriding procedure Read
     (Stream : in out Stream_Type; Item : out Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
       with Post =>
         (declare
            Num_Read : constant Stream_Element_Count :=
              Stream_Element_Count'Min
                (Element_Count (Stream)'Old, Item'Length);
          begin
            Last = Num_Read + Item'First - 1
              and
            Element_Count (Stream) =
              Element_Count (Stream)'Old - Num_Read);

   overriding procedure Write
     (Stream : in out Stream_Type; Item : Stream_Element_Array) with
      Post => Element_Count (Stream) =
      Element_Count (Stream)'Old + Item'Length;

   overriding function Element_Count
     (Stream : Stream_Type) return Stream_Element_Count with
      Post => Element_Count'Result <= Stream.Max_Elements;

   overriding procedure Clear (Stream : in out Stream_Type) with
      Post => Element_Count (Stream) = 0;

private

   type Stream_Type (Max_Elements : Stream_Element_Count) is
     new Storage_Stream_Type with record
      Count : Stream_Element_Count := 0;
      Elements : Stream_Element_Array (1 .. Max_Elements);
   end record;

end Ada.Streams.Storage.Bounded;
