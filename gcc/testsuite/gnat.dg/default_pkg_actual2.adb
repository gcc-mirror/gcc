--  { dg-do compile }

procedure Default_Pkg_Actual2 is

   generic
   package P1 is
   end;

   generic
      with package FP1a is new P1;
      with package FP1b is new P1;
   package P2 is
   end;

   generic
      with package FP2 is new P2 (FP1a => <>,  FP1b => <>);
   package P3 is
   end;

   package NP1a is new P1;
   package NP1b is new P1;
   package NP2  is new P2 (NP1a, NP1b);
   package NP4  is new P3 (NP2);

begin
   null;
end;
