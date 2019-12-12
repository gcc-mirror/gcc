--  { dg-do compile }

with Predicate2.Project.Typ.Set;

procedure Predicate2_Main is
   Type_Def : Predicate2.Project.Typ.Object := Predicate2.Project.Typ.Undefined;
   Types    : Predicate2.Project.Typ.Set.Object;
begin
   Type_Def := Types ("toto");
end Predicate2_Main;
