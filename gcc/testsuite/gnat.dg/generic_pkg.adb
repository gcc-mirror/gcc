--  { dg-do compile }

procedure Generic_Pkg is
   generic
      type T_horizontal is new float;
   package vectors_2D is end;

   generic
      with package C is new vectors_2d (<>);
      with package D is new vectors_2d (<>);
   package poshelp is end;

   generic
      with package Helper is new poshelp (<>);
   package timevars is
      use Helper.C;
   end;

   generic
      with package C is new vectors_2d (<>);
      with package D is new vectors_2d (<>);
      with package Helper is new poshelp (C, D);
   package Spagett is end;

   generic
      with package C is new vectors_2d (<>);
      with package D is new vectors_2d (<>);
      with package Helper is new poshelp (C, D);
   package Touch is
      use Helper;
      package My_Spagett is new Spagett (C, D, Helper);
      package timevars_Pkg is new timevars (Helper);
   end;

begin
  null;
end;
