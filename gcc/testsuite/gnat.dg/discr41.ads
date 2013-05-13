package Discr41 is

   type Vector is array (Positive range <>) of Long_Float;

   type Date is record
      LF : Long_Float := 0.0;
   end record;

   type Date_Vector is array (Positive range <>) of Date;

   type Rec (D : Natural) is record
      B1 : Boolean := False;
      DL : Date_Vector (1 .. D);
      VL : Vector (1 .. D) := (others => 0.0);
      B2 : Boolean := True;
   end record;

   function F return Rec;

end Discr41;
