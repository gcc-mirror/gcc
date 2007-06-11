--  { dg-do compile }
--  { dg-options "-gnatws" }

with G_tables;
procedure test_tables is
   package Inst is new G_Tables (Integer);
   use Inst;
   It : Inst.Table := Create (15);
begin
   null;
end;
