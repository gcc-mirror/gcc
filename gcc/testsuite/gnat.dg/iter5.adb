--  { dg-do compile }

with Iter5_Pkg;

procedure Iter5 is
begin
   for The_Filename of Iter5_Pkg.Iterator_For ("C:\Program_Files") loop
      null;
   end loop;
end Iter5;
