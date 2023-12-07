--  { dg-do compile }
--  { dg-require-effective-target strub }

with Strub_Attr;
procedure Strub_Var1 is
   type TA  -- { dg-warning "does not apply to elements" }
      is array (1..2) of Integer;
   pragma Machine_Attribute (TA, "strub");
   
   A : TA := (0, 0);  -- { dg-warning "does not apply to elements" }
   
   type TR is record  -- { dg-warning "does not apply to fields" }
      M, N : Integer;
   end record;
   pragma Machine_Attribute (TR, "strub");
   
   R : TR := (0, 0);

begin
   A(2) := Strub_Attr.F (A(1));
end Strub_Var1;
