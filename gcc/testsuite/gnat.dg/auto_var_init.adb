-- { dg-do run }
-- { dg-options "-ftrivial-auto-var-init=zero" }

with Ada.Text_IO; use Ada.Text_IO;

procedure Auto_Var_Init is
begin
  Put_Line ("Hello World!");
end;
