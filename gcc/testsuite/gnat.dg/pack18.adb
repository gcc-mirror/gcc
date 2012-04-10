-- { dg-do run }

with Pack18_Pkg; use Pack18_Pkg;

procedure Pack18 is
   use Pack18_Pkg.Attributes_Tables;
   Table : Instance;
begin
   Init (Table);
   Set_Last (Table, 1);
   Table.Table (Last (Table)).N := 0;
end;
