with Interfaces;

package Bit_Packed_Array is

   type laser_illuminator_code_group_t is (zero, one);
   pragma Convention (C, laser_illuminator_code_group_t);

   subtype lic_array_index_t is Interfaces.Unsigned_8 range 0 .. 3;

   type lic_array_t is array (lic_array_index_t) of laser_illuminator_code_group_t;
   pragma Convention (C, lic_array_t);

   type Eighty_Bytes_T is array (1 .. 80) of Interfaces.Unsigned_8;

   type Mission_Assignment_T is record
      Eighty_Bytes           : Eighty_Bytes_T;
      Laser_Illuminator_Code : lic_array_t;
   end record;

   for Mission_Assignment_T use record
      Eighty_Bytes           at 0 range   0 .. 639;
      Laser_Illuminator_Code at 0 range 653 .. 780;
   end record;

   type Mission_Assignment_Dbase_Rec_T is record
      ISF : Mission_Assignment_T;
   end record;

   MADR : Mission_Assignment_Dbase_Rec_T;

   procedure Generate_Callforward;

end Bit_Packed_Array; 
