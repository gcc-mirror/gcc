--  { dg-do compile }
--  { dg-options "-O2" }

with Inline15_Gen;

procedure Inline15 is
   package Inst is new Inline15_Gen;

begin
   Inst.Call_Func;
end Inline15;
