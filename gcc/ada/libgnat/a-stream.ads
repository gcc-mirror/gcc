------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                           A D A . S T R E A M S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package Ada.Streams is
   pragma Pure;

   type Root_Stream_Type is abstract tagged limited private;
   pragma Preelaborable_Initialization (Root_Stream_Type);

   type Stream_Element is mod 2 ** Standard'Storage_Unit;

   type Stream_Element_Offset is new Long_Long_Integer;
   --  Stream_Element_Offset needs 64 bits to accommodate large stream files.
   --  However, rather than make this explicitly 64-bits we derive from
   --  Long_Long_Integer. In normal usage this will have the same effect.
   --  But in the case of CodePeer with a target configuration file with a
   --  maximum integer size of 32, it allows analysis of this unit.

   subtype Stream_Element_Count is
      Stream_Element_Offset range 0 .. Stream_Element_Offset'Last;

   type Stream_Element_Array is
      array (Stream_Element_Offset range <>) of aliased Stream_Element;

   procedure Read
     (Stream : in out Root_Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is abstract;

   procedure Write
     (Stream : in out Root_Stream_Type;
      Item   : Stream_Element_Array)
   is abstract;

private

   type Root_Stream_Type is abstract tagged limited null record;

   --  Stream attributes for Stream_Element_Array: trivially call the
   --  corresponding stream primitive for the whole array, instead of doing
   --  so element by element.

   procedure Read_SEA
     (S : access Root_Stream_Type'Class;
      V : out Stream_Element_Array);

   procedure Write_SEA
     (S : access Root_Stream_Type'Class;
      V : Stream_Element_Array);

   for Stream_Element_Array'Read use Read_SEA;
   for Stream_Element_Array'Write use Write_SEA;

end Ada.Streams;
