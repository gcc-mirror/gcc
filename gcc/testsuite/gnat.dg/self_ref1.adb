--  { dg-do compile }

procedure Self_Ref1 is
   type Integer_Ptr is access all Integer;
   Ptr : constant Integer_Ptr := Integer_Ptr (Ptr); --  { dg-error "object \"Ptr\" cannot be used before end of its declaration" }

begin
   if Ptr /= null then
      null;
   end if;
end Self_Ref1;
