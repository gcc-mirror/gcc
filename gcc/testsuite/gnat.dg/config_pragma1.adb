--  { dg-do run }
--  { dg-options "-gnata" }

with Ada.Strings.Fixed;  use Ada.Strings.Fixed;
with Config_Pragma1_Pkg; use Config_Pragma1_Pkg;

procedure Config_Pragma1 is
   Target : String10;

begin
   for I in Positive10 loop
      Move
        (Source  => Positive10'Image(I),
         Target  => Target);

      FHM.Include
        (Container => FHMM,
         Key       => Target,
         New_Item  => I);
   end loop;
end Config_Pragma1;
