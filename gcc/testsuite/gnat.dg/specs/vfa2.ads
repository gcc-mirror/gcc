-- { dg-do compile }
-- { dg-options "-O" }

package VFA2 is

   type Bit is mod 2**1
     with Size => 1;
   type UInt2 is mod 2**2
     with Size => 2;
   type UInt22 is mod 2**22
     with Size => 22;

   type MODE_ENUM is
     (
      Function_0_Default,
      Function_1,
      Function_2,
      Function_3,
      Function_4,
      Function_5,
      Function_6,
      Function_7)
     with Size => 3;

   type EPD_ENUM is
     (
      Disable_Pull_Down,
      Enable_Pull_Down)
     with Size => 1;

   type EPUN_ENUM is
     (
      Enable_Pull_Up,
      Disable_Pull_Up)
     with Size => 1;

   type EHS_ENUM is
     (
      Slow_Low_Noise_With,
      Fast_Medium_Noise_W)
     with Size => 1;

   type EZI_ENUM is
     (
      Disable_Input_Buffer,
      Enable_Input_Buffer)
     with Size => 1;

   type ZIF_ENUM is
     (
      Enable_Input_Glitch,
      Disable_Input_Glitch)
     with Size => 1;

   type EHD_ENUM is
     (
      Normal_Drive_4_Ma_D,
      Medium_Drive_8_Ma_D,
      High_Drive_14_Ma_Dr,
      Ultra_High_Drive_20)
     with Size => 2;

   type Pin_Type is (Normal_Drive, High_Drive, High_Speed);

   type SFS_Register(Pin : Pin_Type := Normal_Drive) is record
      MODE     : MODE_ENUM;
      EPD      : EPD_ENUM;
      EPUN     : EPUN_ENUM;
      EZI      : EZI_ENUM;
      ZIF      : ZIF_ENUM;
      RESERVED : UInt22;

      case Pin is
         when Normal_Drive =>

            ND_EHS_RESERVED : Bit;
            ND_EHD_RESERVED : UInt2;

         when High_Drive =>

            EHD : EHD_ENUM;
            HD_EHS_RESERVED : Bit;

         when High_Speed =>
            EHS    : EHS_ENUM;
            HS_EHD_RESERVED : UInt2;

      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access;

   for SFS_Register use record
      MODE            at 0 range 0 .. 2;
      EPD             at 0 range 3 .. 3;
      EPUN            at 0 range 4 .. 4;
      ND_EHS_RESERVED at 0 range 5 .. 5;
      HD_EHS_RESERVED at 0 range 5 .. 5;
      EHS             at 0 range 5 .. 5;
      EZI             at 0 range 6 .. 6;
      ZIF             at 0 range 7 .. 7;
      ND_EHD_RESERVED at 0 range 8 .. 9;
      EHD             at 0 range 8 .. 9;
      HS_EHD_RESERVED at 0 range 8 .. 9;
      RESERVED        at 0 range 10 .. 31;
   end record;

   type Normal_Drive_Pins is array (Integer range <>)
     of SFS_Register(Normal_Drive) with Volatile;

end VFA2;
