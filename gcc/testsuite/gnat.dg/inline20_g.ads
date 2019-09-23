with Ada.Streams;

generic
package Inline20_G is

   subtype Offset_Type is Ada.Streams.Stream_Element_Offset;

   generic
      type T is private;
   package Nested_G is

      procedure Get (Data : T; Into : out Offset_Type);

      function F return Integer with Inline;

   end Nested_G;

end Inline20_G;
