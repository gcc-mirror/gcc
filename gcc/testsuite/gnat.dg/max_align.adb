--  { dg-do compile }

procedure Max_Align is
   type Block is record
      X : Integer;
   end record;
   for Block'Alignment use Standard'Maximum_Alignment;

   type Block_Access is access Block;
   Ptr : Block_Access := new Block;
begin
   null;
end;


