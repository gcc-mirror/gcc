-- { dg-do compile }
-- { dg-options "-Wuninitialized" }

pragma Warnings (Off);

function Warn9 return Integer is
  I : Integer;
begin
  return I;
end;
