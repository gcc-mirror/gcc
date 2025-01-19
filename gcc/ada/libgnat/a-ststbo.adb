------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--           A D A . S T R E A M S . S T O R A G E . B O U N D E D          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2020-2025, Free Software Foundation, Inc.      --
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

package body Ada.Streams.Storage.Bounded is

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out Stream_Type; Item : out Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
   is
      EA : Stream_Element_Array renames
        Stream.Elements (1 .. Element_Count (Stream));
   begin
      if Item'Length = 0 then
         Last := Item'First - 1;

      --  If the entire content of the stream fits in Item, then copy it and
      --  clear the stream. This is likely the usual case.

      elsif Element_Count (Stream) <= Item'Length then
         Last := Item'First + Element_Count (Stream) - 1;
         Item (Item'First .. Last) := EA;
         Clear (Stream);

      --  Otherwise, copy as much into Item as will fit. Then slide the
      --  remaining part of the stream down, and compute the new Count.
      --  We expect this to be the unusual case, so the cost of copying
      --  the remaining part probably doesn't matter.

      else
         Last := Item'Last;

         declare
            New_Count : constant Stream_Element_Count :=
              Element_Count (Stream) - Item'Length;
         begin
            Item := EA (1 .. Item'Length);
            EA (1 .. New_Count) :=
              EA (Element_Count (Stream) - New_Count + 1 ..
                  Element_Count (Stream));
            Stream.Count := New_Count;
         end;
      end if;
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Stream_Type; Item : Stream_Element_Array)
   is
   begin
      if Element_Count (Stream) + Item'Length > Stream.Max_Elements then
         --  That is a precondition in the RM
         raise Constraint_Error;
      end if;

      declare
         New_Count : constant Stream_Element_Count :=
           Element_Count (Stream) + Item'Length;
      begin
         Stream.Elements (Element_Count (Stream) + 1 .. New_Count) := Item;
         Stream.Count := New_Count;
      end;
   end Write;

   -------------------
   -- Element_Count --
   -------------------

   overriding function Element_Count
     (Stream : Stream_Type) return Stream_Element_Count
   is
   begin
      return Stream.Count;
   end Element_Count;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Stream : in out Stream_Type)
   is
   begin
      Stream.Count := 0;
   end Clear;

end Ada.Streams.Storage.Bounded;
