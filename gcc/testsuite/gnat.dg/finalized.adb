-- { dg-do compile }
-- { dg-options "-gnatws" }

with Ada.Finalization; use Ada.Finalization;
procedure finalized is
   type Rec is new Controlled with null record;
   Obj : access Rec := new Rec'(Controlled with null record);
begin
   null;
end;
