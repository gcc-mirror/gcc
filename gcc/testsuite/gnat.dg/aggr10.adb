-- { dg-do compile }
-- { dg-options "-O2" }

with Aggr10_Pkg; use Aggr10_Pkg;

procedure Aggr10 is

   No_Name_Location : constant Name_Location :=
                        (Name     => Name_Id'First,
                         Location => Int'First,
                         Source   => Source_Id'First,
                         Except   => False,
                         Found    => False);

   Name_Loc : Name_Location;

begin
   Name_Loc := Get;
   if Name_Loc = No_Name_Location then  -- { dg-bogus "comparison always false" }
      raise Program_Error;
   end if;
   Set (Name_Loc);
end;
