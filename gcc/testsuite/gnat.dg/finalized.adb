-- { dg-do compile }

pragma Warnings
  (Off, "anonymous access-to-controlled object will be finalized when its enclosing unit goes out of scope");

with Ada.Finalization; use Ada.Finalization;
procedure finalized is
   type Rec is new Controlled with null record;
   Obj : access Rec := new Rec'(Controlled with null record);
begin
   null;
end;
