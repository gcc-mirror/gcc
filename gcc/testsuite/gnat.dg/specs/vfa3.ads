-- { dg-do compile }

package VFA3 is

   type Bit is mod 2**1
     with Size => 1;

   type Intr_Level_Field is record
      Level_Low  : Bit := 0;
      Level_High : Bit := 0;
      Edge_Low   : Bit := 0;
      Edge_High  : Bit := 0;
   end record with Pack, Size => 4;
   for Intr_Level_Field use record
      Level_Low  at 0 range 0 .. 0;
      Level_High at 0 range 1 .. 1;
      Edge_Low   at 0 range 2 .. 2;
      Edge_High  at 0 range 3 .. 3;
   end record;

   type Intr_Level_Cluster is array (0 .. 7) of Intr_Level_Field
     with Volatile_Full_Access, Pack, Object_Size => 32;
   --  There are 8 Fields in a 32-bit word.

end VFA3;
