--  { dg-do compile }
--  { dg-options "-gnatwu" }

with Ada.Numerics.Discrete_Random; use Ada.Numerics;

procedure Entry_Family is
   protected Family is
      entry Call (Boolean);
   end Family;

   protected body Family is
      entry Call (for P in Boolean) when True is
      begin
         null;
      end Call;

   end Family;

   package Random_Boolean is new Discrete_Random (Result_Subtype => Boolean);
   use Random_Boolean;

   Boolean_Generator : Generator;

   B : constant Boolean := Random (Boolean_Generator);

begin
   Family.Call (B);
end Entry_Family;
