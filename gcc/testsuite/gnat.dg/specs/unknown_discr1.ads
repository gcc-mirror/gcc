-- { dg-do compile }

with Unknown_Discr1_Pkg; use Unknown_Discr1_Pkg;
with Unknown_Discr1_Pkg.Child;
with Unknown_Discr1_Pkg.Inst;

package Unknown_Discr1 is

  A : Tagged_Type (0); -- { dg-error "type has unknown discriminants" }

  B : Child.Derived_1 (1); -- { dg-error "type has unknown discriminants" }

  C : Child.Derived_2 (2); -- { dg-error "type has unknown discriminants" }

  D : Child.Nested.Derived_3 (3); -- { dg-error "type has unknown discriminants" }

  E : Inst.Derived_1 (1); -- { dg-error "type has unknown discriminants" }

  F : Inst.Derived_2 (2); -- { dg-error "type has unknown discriminants" }

  G : Inst.Nested.Derived_3 (3); -- { dg-error "type has unknown discriminants" }

end Unknown_Discr1;
