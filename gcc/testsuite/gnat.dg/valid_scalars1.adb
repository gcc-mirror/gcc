--  { dg-do compile }
--  { dg-options "-gnata -gnatws" }

procedure Valid_Scalars1 is
   type Ptr is access Integer;
   V1 : Ptr;

   Check : Boolean := V1'Valid_Scalars;
begin
   pragma Assert (Check);
end;
