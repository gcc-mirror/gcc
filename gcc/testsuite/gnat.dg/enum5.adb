--  { dg-do compile }

procedure Enum5 is
   package Pack1 is
      type Enum is (A, B, C);
   end;
   E1 : Pack1.Enum;
   use all type Pack1.Enum;
begin
   E1 := A;
end;
