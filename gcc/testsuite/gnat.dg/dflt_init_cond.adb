--  { dg-do compile }

with Dflt_Init_Cond_Pkg; use Dflt_Init_Cond_Pkg;

procedure Dflt_Init_Cond is
   E : Explicit;
   I : Implicit;

begin
   Read (E);
   Read (I);
end Dflt_Init_Cond;
