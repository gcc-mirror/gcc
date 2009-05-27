-- { dg-do run }
-- { dg-options "-gnatp" }

procedure Misaligned_Nest is

   type Int is record
      V : Integer;
   end record;

   type Block is record
      B : Boolean;
      I : Int;
   end record;
   pragma Pack (Block);
   for Block'Alignment use 1;

   type Pair is array (1 .. 2) of Block;

   P : Pair;
begin
   for K in P'Range loop
      P(K).I.V := 1;
   end loop;
end;


