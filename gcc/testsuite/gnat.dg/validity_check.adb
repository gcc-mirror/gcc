--  { dg-do run }
--  { dg-options "-cargs -O -gnatn -gnatVa -gnatws -margs" }

pragma Initialize_Scalars;

procedure Validity_Check is

   type Small_Int is mod 2**6;

   type Arr is array (1 .. 16) of Small_Int;
   pragma Pack (Arr);

   S : Small_Int;
   A : Arr;

begin
   null;
end;
