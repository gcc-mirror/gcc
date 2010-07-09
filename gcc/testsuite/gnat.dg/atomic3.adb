-- { dg-do compile }

procedure Atomic3 is

   type Unsigned_32_T is mod 2 ** 32;
   for Unsigned_32_T'Size use 32;

   type Id_T is (One, Two, Three);

   type Array_T is array (Id_T) of Unsigned_32_T;
   pragma Atomic_Components (Array_T);

   A : Array_T := (others => 0);

   function Get_Array return Array_T is
   begin
      return A;
   end;

   X : Array_T;

begin
   X := Get_Array;
end;
