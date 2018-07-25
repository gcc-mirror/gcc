--  { dg-do compile }
--  { dg-options "-gnatwr" }

procedure Others1 is
   type Ar is Array (1..10) of Natural;
   function five return integer is (5);
   THing : Ar;
begin
   Thing := (1..5 => 22, 6 ..10 => 111, others => Five); --  { dg-warning "there are no others" }
   if Thing (1) /= thing (5) then
      raise Program_Error;
   end if;
end;
