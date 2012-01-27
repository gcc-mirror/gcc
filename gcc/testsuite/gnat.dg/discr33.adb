-- { dg-do run }

procedure Discr33 is

   subtype Int is Integer range 1..100;

   type T (D : Int := 1) is
      record
         A : Integer;
         B : String (1..D);
         C : aliased Integer;
      end record;

   Var : T := (D => 1, A => 1234, B => "x", C => 4567);

   type Int_Ref is access all Integer;
   Pointer_To_C : Int_Ref := Var.C'Access;

begin

   if Pointer_To_C.all /= 4567 then
      raise Program_Error;
   end if;

   Var := (D => 26, A => 1234, B => "abcdefghijklmnopqrstuvwxyz", C => 2345);

   if Pointer_To_C.all /= 2345 then
      raise Program_Error;
   end if;

end Discr33;
