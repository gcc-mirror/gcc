with Ada.Streams; use Ada.Streams;

package body Inline20_G is

   package body Nested_G is

      procedure Get (Data : T; Into : out Offset_Type) is
      begin
         Into := (T'Descriptor_Size + Data'Size) / Standard'Storage_Unit;
      end;

      function F return Integer is
      begin
         return 0;
      end;
   end Nested_G;

end Inline20_G;
