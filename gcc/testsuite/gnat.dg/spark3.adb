--  { dg-do compile }

procedure SPARK3 (X : in out Integer) with SPARK_Mode is

   procedure Q (X : in out Integer) with SPARK_Mode => Off is
   begin
      X := X + 1;
   end Q;

   procedure R (X : in out Integer);

   procedure R (X : in out Integer) with SPARK_Mode => Off is
   begin
      Q (X);
   end R;

begin
   R (X);
   X := X + 1;
end SPARK3;
