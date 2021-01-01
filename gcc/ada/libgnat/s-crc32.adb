------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                        S Y S T E M  . C R C 3 2                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2021, Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

pragma Compiler_Unit_Warning;

package body System.CRC32 is

   Init   : constant CRC32 := 16#FFFF_FFFF#;  -- Initial value
   XorOut : constant CRC32 := 16#FFFF_FFFF#;  -- To compute final result.

   --  The following table contains precomputed values for contributions
   --  from various possible byte values. Doing a table lookup is quicker
   --  than processing the byte bit by bit.

   Table : constant array (CRC32 range 0 .. 255) of CRC32 :=
     (16#0000_0000#, 16#7707_3096#, 16#EE0E_612C#, 16#9909_51BA#,
      16#076D_C419#, 16#706A_F48F#, 16#E963_A535#, 16#9E64_95A3#,
      16#0EDB_8832#, 16#79DC_B8A4#, 16#E0D5_E91E#, 16#97D2_D988#,
      16#09B6_4C2B#, 16#7EB1_7CBD#, 16#E7B8_2D07#, 16#90BF_1D91#,
      16#1DB7_1064#, 16#6AB0_20F2#, 16#F3B9_7148#, 16#84BE_41DE#,
      16#1ADA_D47D#, 16#6DDD_E4EB#, 16#F4D4_B551#, 16#83D3_85C7#,
      16#136C_9856#, 16#646B_A8C0#, 16#FD62_F97A#, 16#8A65_C9EC#,
      16#1401_5C4F#, 16#6306_6CD9#, 16#FA0F_3D63#, 16#8D08_0DF5#,
      16#3B6E_20C8#, 16#4C69_105E#, 16#D560_41E4#, 16#A267_7172#,
      16#3C03_E4D1#, 16#4B04_D447#, 16#D20D_85FD#, 16#A50A_B56B#,
      16#35B5_A8FA#, 16#42B2_986C#, 16#DBBB_C9D6#, 16#ACBC_F940#,
      16#32D8_6CE3#, 16#45DF_5C75#, 16#DCD6_0DCF#, 16#ABD1_3D59#,
      16#26D9_30AC#, 16#51DE_003A#, 16#C8D7_5180#, 16#BFD0_6116#,
      16#21B4_F4B5#, 16#56B3_C423#, 16#CFBA_9599#, 16#B8BD_A50F#,
      16#2802_B89E#, 16#5F05_8808#, 16#C60C_D9B2#, 16#B10B_E924#,
      16#2F6F_7C87#, 16#5868_4C11#, 16#C161_1DAB#, 16#B666_2D3D#,
      16#76DC_4190#, 16#01DB_7106#, 16#98D2_20BC#, 16#EFD5_102A#,
      16#71B1_8589#, 16#06B6_B51F#, 16#9FBF_E4A5#, 16#E8B8_D433#,
      16#7807_C9A2#, 16#0F00_F934#, 16#9609_A88E#, 16#E10E_9818#,
      16#7F6A_0DBB#, 16#086D_3D2D#, 16#9164_6C97#, 16#E663_5C01#,
      16#6B6B_51F4#, 16#1C6C_6162#, 16#8565_30D8#, 16#F262_004E#,
      16#6C06_95ED#, 16#1B01_A57B#, 16#8208_F4C1#, 16#F50F_C457#,
      16#65B0_D9C6#, 16#12B7_E950#, 16#8BBE_B8EA#, 16#FCB9_887C#,
      16#62DD_1DDF#, 16#15DA_2D49#, 16#8CD3_7CF3#, 16#FBD4_4C65#,
      16#4DB2_6158#, 16#3AB5_51CE#, 16#A3BC_0074#, 16#D4BB_30E2#,
      16#4ADF_A541#, 16#3DD8_95D7#, 16#A4D1_C46D#, 16#D3D6_F4FB#,
      16#4369_E96A#, 16#346E_D9FC#, 16#AD67_8846#, 16#DA60_B8D0#,
      16#4404_2D73#, 16#3303_1DE5#, 16#AA0A_4C5F#, 16#DD0D_7CC9#,
      16#5005_713C#, 16#2702_41AA#, 16#BE0B_1010#, 16#C90C_2086#,
      16#5768_B525#, 16#206F_85B3#, 16#B966_D409#, 16#CE61_E49F#,
      16#5EDE_F90E#, 16#29D9_C998#, 16#B0D0_9822#, 16#C7D7_A8B4#,
      16#59B3_3D17#, 16#2EB4_0D81#, 16#B7BD_5C3B#, 16#C0BA_6CAD#,
      16#EDB8_8320#, 16#9ABF_B3B6#, 16#03B6_E20C#, 16#74B1_D29A#,
      16#EAD5_4739#, 16#9DD2_77AF#, 16#04DB_2615#, 16#73DC_1683#,
      16#E363_0B12#, 16#9464_3B84#, 16#0D6D_6A3E#, 16#7A6A_5AA8#,
      16#E40E_CF0B#, 16#9309_FF9D#, 16#0A00_AE27#, 16#7D07_9EB1#,
      16#F00F_9344#, 16#8708_A3D2#, 16#1E01_F268#, 16#6906_C2FE#,
      16#F762_575D#, 16#8065_67CB#, 16#196C_3671#, 16#6E6B_06E7#,
      16#FED4_1B76#, 16#89D3_2BE0#, 16#10DA_7A5A#, 16#67DD_4ACC#,
      16#F9B9_DF6F#, 16#8EBE_EFF9#, 16#17B7_BE43#, 16#60B0_8ED5#,
      16#D6D6_A3E8#, 16#A1D1_937E#, 16#38D8_C2C4#, 16#4FDF_F252#,
      16#D1BB_67F1#, 16#A6BC_5767#, 16#3FB5_06DD#, 16#48B2_364B#,
      16#D80D_2BDA#, 16#AF0A_1B4C#, 16#3603_4AF6#, 16#4104_7A60#,
      16#DF60_EFC3#, 16#A867_DF55#, 16#316E_8EEF#, 16#4669_BE79#,
      16#CB61_B38C#, 16#BC66_831A#, 16#256F_D2A0#, 16#5268_E236#,
      16#CC0C_7795#, 16#BB0B_4703#, 16#2202_16B9#, 16#5505_262F#,
      16#C5BA_3BBE#, 16#B2BD_0B28#, 16#2BB4_5A92#, 16#5CB3_6A04#,
      16#C2D7_FFA7#, 16#B5D0_CF31#, 16#2CD9_9E8B#, 16#5BDE_AE1D#,
      16#9B64_C2B0#, 16#EC63_F226#, 16#756A_A39C#, 16#026D_930A#,
      16#9C09_06A9#, 16#EB0E_363F#, 16#7207_6785#, 16#0500_5713#,
      16#95BF_4A82#, 16#E2B8_7A14#, 16#7BB1_2BAE#, 16#0CB6_1B38#,
      16#92D2_8E9B#, 16#E5D5_BE0D#, 16#7CDC_EFB7#, 16#0BDB_DF21#,
      16#86D3_D2D4#, 16#F1D4_E242#, 16#68DD_B3F8#, 16#1FDA_836E#,
      16#81BE_16CD#, 16#F6B9_265B#, 16#6FB0_77E1#, 16#18B7_4777#,
      16#8808_5AE6#, 16#FF0F_6A70#, 16#6606_3BCA#, 16#1101_0B5C#,
      16#8F65_9EFF#, 16#F862_AE69#, 16#616B_FFD3#, 16#166C_CF45#,
      16#A00A_E278#, 16#D70D_D2EE#, 16#4E04_8354#, 16#3903_B3C2#,
      16#A767_2661#, 16#D060_16F7#, 16#4969_474D#, 16#3E6E_77DB#,
      16#AED1_6A4A#, 16#D9D6_5ADC#, 16#40DF_0B66#, 16#37D8_3BF0#,
      16#A9BC_AE53#, 16#DEBB_9EC5#, 16#47B2_CF7F#, 16#30B5_FFE9#,
      16#BDBD_F21C#, 16#CABA_C28A#, 16#53B3_9330#, 16#24B4_A3A6#,
      16#BAD0_3605#, 16#CDD7_0693#, 16#54DE_5729#, 16#23D9_67BF#,
      16#B366_7A2E#, 16#C461_4AB8#, 16#5D68_1B02#, 16#2A6F_2B94#,
      16#B40B_BE37#, 16#C30C_8EA1#, 16#5A05_DF1B#, 16#2D02_EF8D#);

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (C : CRC32) return Interfaces.Unsigned_32 is
   begin
      return Interfaces.Unsigned_32 (C xor XorOut);
   end Get_Value;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (C : out CRC32) is
   begin
      C := Init;
   end Initialize;

   ------------
   -- Update --
   ------------

   procedure Update (C : in out CRC32; Value : Character) is
      V : constant CRC32 := CRC32 (Character'Pos (Value));
   begin
      C := Shift_Right (C, 8) xor Table (V xor (C and 16#0000_00FF#));
   end Update;

end System.CRC32;
