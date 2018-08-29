--  { dg-do compile }

with Ada.Containers.Indefinite_Holders;

procedure Discr51 is

   package Inner is
      type Str (<>) is private;
   private
      type Str is array (Positive range <>) of Character;
   end Inner;

   package Inner2 is
      type Str2 (<>) is private;
   private
      type str2 is new inner.Str;
   end Inner2;

   type Str3 is new Inner.str;

   package Str_Holders is new Ada.Containers.Indefinite_Holders
      (Inner.Str, Inner."=");

   package Str2_Holders is new Ada.Containers.Indefinite_Holders
      (Inner2.Str2, Inner2."=");

   package Str3_Holders is new Ada.Containers.Indefinite_Holders
      (Str3, "=");

begin
   null;
end Discr51;
