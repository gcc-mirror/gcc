-- { dg-do run }
procedure Check_Displace_Generation is

   package Stuff is

      type Base_1 is interface;
      function F_1 (X : Base_1) return Integer is abstract;

      type Base_2 is interface;
      function F_2 (X : Base_2) return Integer is abstract;

      type Concrete is new Base_1 and Base_2 with null record;
      function F_1 (X : Concrete) return Integer;
      function F_2 (X : Concrete) return Integer;

   end Stuff;

   package body Stuff is

      function F_1 (X : Concrete) return Integer is
      begin
         return 1;
      end F_1;

      function F_2 (X : Concrete) return Integer is
      begin
         return 2;
      end F_2;

   end Stuff;

   use Stuff;

   function Make_Concrete return Concrete is
      C : Concrete;
   begin
      return C;
   end Make_Concrete;

   B_1 : Base_1'Class := Make_Concrete;
   B_2 : Base_2'Class := Make_Concrete;

begin
   if B_1.F_1 /= 1 then
      raise Program_Error with "bad B_1.F_1 call";
   end if;
   if B_2.F_2 /= 2 then
      raise Program_Error with "bad B_2.F_2 call";
   end if;
end Check_Displace_Generation;
