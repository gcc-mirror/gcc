-- { dg-do compile }
-- { dg-options "-O2" }

function slice1 (Offset : Integer) return String is
   
   Convert : constant String := "0123456789abcdef";
   Buffer  : String (1 .. 32);
   Pos     : Natural := Buffer'Last;
   Value   : Long_Long_Integer := Long_Long_Integer (Offset);

begin
   while Value > 0 loop
      Buffer (Pos) := Convert (Integer (Value mod 16));
      Pos := Pos - 1;
      Value := Value / 16;
   end loop;
   
   return Buffer (Pos + 1 .. Buffer'Last);
end;
