-- { dg-do compile }
-- { dg-options "-O -gnatn" }

with Ada.Text_IO; use Ada.Text_IO;
with Controlled6_Pkg;
with Controlled6_Pkg.Iterators;

procedure Controlled6 is

   type String_Access is access String;

   package My_Q is new Controlled6_Pkg (String_Access);
   package My_Iterators is new My_Q.Iterators (0);
   use My_Iterators;

   Iterator : Iterator_Type := Find;

begin
   loop
      exit when Is_Null (Iterator);
      Put (Current (Iterator).all & ' ');
      Find_Next (Iterator);
   end loop;
end;
