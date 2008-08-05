--  { dg-do compile }
--  { dg-options "-gnata" }

procedure Post_Block is
   package Pack is
      function Size (X : Integer) return Integer;
      pragma Postcondition (Size'Result = Value (X)'Length);   --  OK
      pragma Postcondition (Value (X)'Length = Size'Result);

      --  Calling the following requires a transient block.
      function Value (X : Integer) return String;
   end Pack;
   
   package body Pack is
      function Size (X : Integer) return Integer is
      begin
         return 0;
      end;
      
      function Value (X : Integer) return String is
      begin
         return Integer'image (X);
      end;
   end Pack;
begin
   null;
end;
