--  { dg-do compile }

with Interfaces.C;

procedure Warn15.Core.Main is
   use type Interfaces.C.unsigned;  --  { dg-error "\"C\" not declared in \"Interfaces\"" }
begin
   null;
end Warn15.Core.Main;
