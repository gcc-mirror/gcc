--  { dg-do run }

with Class_Wide4_Pkg;
with Class_Wide4_Pkg2;

procedure Class_Wide4 is
   D  : aliased Class_Wide4_Pkg.Data_Object;
   O  : aliased Class_Wide4_Pkg2.Object;
   IA : not null access Class_Wide4_Pkg.Conditional_Interface'Class :=
          O'Access;
   I  : Class_Wide4_Pkg.Conditional_Interface'Class renames
          Class_Wide4_Pkg.Conditional_Interface'Class (O);
begin
   O.Do_Stuff;
   O.Do_Stuff_Access;
   IA.Do_Stuff;
   IA.Do_Stuff_Access;
   I.Do_Stuff;
   I.Do_Stuff_Access;
end Class_Wide4;
