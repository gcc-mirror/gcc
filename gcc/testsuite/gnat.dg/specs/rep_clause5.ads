-- { dg-do compile }

pragma Implicit_Packing;

package Rep_Clause5 is

   type Modes_Type is (Mode_0, Mode_1);
   for  Modes_Type'size use 8;

   type Mode_Record_Type is
      record
         Mode_1 : aliased Modes_Type;
         Mode_2 : aliased Modes_Type;
         Mode_3 : aliased Modes_Type;
         Mode_4 : aliased Modes_Type;
         Time   : aliased Float;
      end record;

   for Mode_Record_Type use
      record
         Mode_1 at 00 range 00 .. 07;
         Mode_2 at 01 range 00 .. 07;
         Mode_3 at 02 range 00 .. 07;
         Mode_4 at 03 range 00 .. 07;
         Time   at 04 range 00 .. 31;
      end record;

   for Mode_Record_Type'Size use 64;
   for Mode_Record_Type'Alignment use 4;

   type Array_1_Type is array (0 .. 31) of Boolean;
   for  Array_1_Type'size use 32;

   type Array_2_Type is array (0 .. 127) of Boolean;
   for  Array_2_Type'size use 128;
   for Array_2_Type'Alignment use 4;

   type Array_3_Type is array (0 .. 31) of Boolean;
   for  Array_3_Type'size use 32;

   type Unsigned_Long is mod 2 ** 32;
   type Array_4_Type is array (1 .. 6) of unsigned_Long;

   type Primary_Data_Type is
      record
         Array_1           : aliased Array_1_Type;
         Mode_Record       : aliased Mode_Record_Type;
         Array_2           : aliased Array_2_Type;
         Array_3           : Array_3_Type;
         Array_4           : Array_4_Type;
      end record;

   for Primary_Data_Type use
      record
         Array_1           at  0 range  0 ..  31; -- WORD 1
         Mode_Record       at  4 range  0 ..  63; -- WORD 2 ..  3
         Array_2           at 12 range  0 .. 127; -- WORD 4 ..  7
         Array_3           at 28 range  0 ..  31; -- WORD 8
         Array_4           at 32 range  0 .. 191; -- WORD 9 .. 14
      end record;

   for Primary_Data_Type'Size use 448;

   type Results_Record_Type is
      record
        Thirty_Two_Bit_Pad : Float;
        Result             : Primary_Data_Type;
      end record;

   for Results_Record_Type use
      record
         Thirty_Two_Bit_Pad at 0 range 0 ..  31;
         Result             at 4 range 0 .. 447;
      end record;

end Rep_Clause5;
