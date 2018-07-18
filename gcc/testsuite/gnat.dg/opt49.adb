-- { dg-do run }
-- { dg-require-stack-check "" }
-- { dg-options "-O -fstack-check" }

procedure Opt49 is

   function Ident (I : Integer) return Integer;
   pragma No_Inline (Ident);

   function Ident (I : Integer) return Integer is
   begin
     return I;
   end;

   Int_0 : Integer := Ident (0);
   Int_4 : Integer := Ident (4);

   A : array (-4 .. Int_4) of Integer;

begin
   A := (-4 , -3 , -2 , -1 , 100 , 1 , 2 , 3 , 4);
   A (-4 .. Int_0) :=  A (Int_0 .. 4); 
   if A /= (100 ,  1 ,  2 ,  3 ,  4  , 1 , 2 , 3 , 4) then
      raise Program_Error;
   end if;

   A := (-4 , -3 , -2 , -1 , 100 ,  1 ,  2 ,  3 ,  4);
   A (Int_0 .. 4) := A (-4 .. Int_0); 
   if A /= (-4 , -3 , -2 , -1 , -4  , -3 , -2 , -1 , 100) then
      raise Program_Error;
   end if;
end;
