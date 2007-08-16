--  { dg-do run }

with addr2_p; use addr2_p;
procedure addr2 is
begin
   Process (B1);
   Process (Blk => B1);
   Process (B2);
   Process (Blk => B2);
end;
