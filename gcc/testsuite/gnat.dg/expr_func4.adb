--  { dg-do compile }

procedure Test_Exp is
   function X return Boolean is
     (Integer'Size = 32) or else (Float'Size = 32);  -- { dg-error "expression function must be enclosed in parentheses" }
begin
   null;
end;
