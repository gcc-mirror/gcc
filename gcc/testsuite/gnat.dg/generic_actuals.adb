--  { dg-do compile }
--  { dg-options "-gnatd.F" }

procedure Generic_Actuals with SPARK_Mode is
   generic
      X : Integer;
      Y : Integer := 0;
   package Q with Initializes => (XX => X, YY => Y) is
      --  Both X and Y actuals can appear in the Initializes contract,
      --  i.e. the default expression of Y should not matter.
      XX : Integer := X;
      YY : Integer := Y;
   end Q;

   package Inst is new Q (X => 0);
begin
   null;
end Generic_Actuals;
