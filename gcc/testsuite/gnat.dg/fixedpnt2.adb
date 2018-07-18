--  { dg-do compile }

with Ada.Unchecked_Conversion;

package body Fixedpnt2 is

   function To_Integer_64 is
      new Ada.Unchecked_Conversion (Source => My_Type,
                                    Target => T_Integer_64);

   function To_T is
      new Ada.Unchecked_Conversion (Source => T_Integer_64,
                                    Target => My_Type);

   function "*" (Left  : in T_Integer_32;
                 Right : in My_Type)
      return My_Type is
         (To_T (S => T_Integer_64 (Left) * To_Integer_64 (S => Right)));

   function "*" (Left  : in My_Type;
                 Right : in T_Integer_32)
      return My_Type is
         (To_T (S => To_Integer_64 (S => Left) * T_Integer_64 (Right)));

end Fixedpnt2;
