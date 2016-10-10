-- { dg-do run }
-- { dg-options "-gnatws" }

procedure Biased_Subtype is

   CIM_Max_AA : constant := 9_999_999;
   CIM_Min_AA : constant := -999_999;

   type TIM_AA is range CIM_Min_AA..CIM_Max_AA + 1;
   for TIM_AA'Size use 24;

   subtype STIM_AA is TIM_AA range TIM_AA(CIM_Min_AA)..TIM_AA(CIM_Max_AA);

   SAA : STIM_AA := 1;

begin
   if Integer(SAA) /= 1 then
     raise Program_Error;
   end if;
end;
