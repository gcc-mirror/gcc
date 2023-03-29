-- { dg-do compile }
-- { dg-options "-O2" }

package Opt6 is

   type Timer_Output_Compare_And_PWM_Mode is
     (Frozen,
      Active,
      Inactive,
      Toggle,
      Force_Inactive,
      Force_Active,
      PWM1,
      PWM2);

   type Timer_Capture_Compare_Modes is
     (Output, Direct_TI, Indirect_TI, TRC);

   type Timer_Input_Capture_Filter is mod 16;

   type Timer_Input_Capture_Prescaler is
     (Div1,
      Div2,
      Div4,
      Div8);

   type Channel_Output_Descriptor is record
      OCxFast_Enable    : Boolean;
      OCxPreload_Enable : Boolean;
      OCxMode           : Timer_Output_Compare_And_PWM_Mode;
      OCxClear_Enable   : Boolean;
   end record with Size => 6;
   for Channel_Output_Descriptor use record
      OCxFast_Enable    at 0 range 0 .. 0;
      OCxPreload_Enable at 0 range 1 .. 1;
      OCxMode           at 0 range 2 .. 4;
      OCxClear_Enable   at 0 range 5 .. 5;
   end record;

   type Channel_Input_Descriptor is record
      ICxFilter    : Timer_Input_Capture_Filter;
      ICxPrescaler : Timer_Input_Capture_Prescaler;
   end record with Size => 6;
   for Channel_Input_Descriptor use record
      ICxFilter    at 0 range 2 .. 5;
      ICxPrescaler at 0 range 0 .. 1;
   end record;

   type IO_Descriptor (CCxSelection : Timer_Capture_Compare_Modes := Output) is
      record
         case CCxSelection is
            when Direct_TI .. TRC =>
               Capture : Channel_Input_Descriptor;
            when Output =>
               Compare : Channel_Output_Descriptor;
         end case;
      end record with Size => 8;
   for IO_Descriptor use record
      CCxSelection at 0 range 0 .. 1;
      Capture      at 0 range 2 .. 7;
      Compare      at 0 range 2 .. 7;
   end record;

   subtype Lower_Half_Index is Integer range 1 .. 2;
   type TIMx_CCMRx_Lower_Half is
     array (Lower_Half_Index) of IO_Descriptor
   with Volatile_Components, Component_Size => 8, Size => 16;

end Opt6;
