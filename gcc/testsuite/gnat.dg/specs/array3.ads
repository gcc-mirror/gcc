-- PR middle-end/56474
-- Reported by Pavel Zhukov <pavel@zhukoff.net>

-- { dg-do compile }

with Ada.Streams;

package Array3 is

   use type Ada.Streams.Stream_Element_Offset;

   type Vector (Size : Ada.Streams.Stream_Element_Offset) is record
      Value : Ada.Streams.Stream_Element_Array (0 .. Size);
   end record;

   Empty_Vector : Vector (-1);

end Array3;
