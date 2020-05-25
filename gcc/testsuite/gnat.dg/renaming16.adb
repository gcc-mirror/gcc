-- { dg-do compile }

with Renaming16_Pkg; use Renaming16_Pkg;

procedure Renaming16 is
   Results : Bindings_Query_Results_Type;
begin
   for I in Create_Bindings_Iterator (Results) loop
      null;
   end loop;
end;
