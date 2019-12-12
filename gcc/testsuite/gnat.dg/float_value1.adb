--  { dg-do run { target i?86-*-* x86_64-*-* } }

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Float_Value1 is
   Str1 : String := "0." & 50000 * "4";
   Str2 : String := "1." & 5000 * "4";
   Str3 : String := "16#0." & 500000 * "4" & "#";
   Str4 : String := "1" & (5000 * "0") & "E-5000";
   Str5 : String := "1" & "." & 50000 * "0" & "1";
   Str6 : String := 50000 * "0" & "." & 50000 * "2" & "1";
   Str7 : String := "1" & (5000 * "0") & "1" & "E-5000";
   Str8 : String := "16#1" & "." & 50000 * "0" & "1#";

   procedure Test (Msg, Str, Expected : String) is
      Number     : Long_Long_Float;
   begin
      Number := Long_Long_Float'Value (Str);
      if Number'Img /= Expected then
         raise Program_Error;
      end if;
   end Test;

begin
   Test ("0.4444...[50000 times]   ", Str1, " 4.44444444444444444E-01");
   Test ("1.4...[5000 times]       ", Str2, " 1.44444444444444444E+00");
   Test ("16#0.[50000 '4']#        ", Str3, " 2.66666666666666667E-01");
   Test ("1[5000 zeros]E-5000      ", Str4, " 1.00000000000000000E+00");
   Test ("1.[50000zeros]1          ", Str5, " 1.00000000000000000E+00");
   Test ("[50000zeros].[50000 '2']1", Str6, " 2.22222222222222222E-01");
   Test ("1[50000zeros]1.E-5000    ", Str7, " 1.00000000000000000E+01");
   Test ("16#1.[50000zeros]1#      ", Str8, " 1.00000000000000000E+00");

   --  Check that number of trailing zero after point does not change
   --  the value

   for J in 1 .. 10000 loop
      declare
         Str : String := "0.1" & J * "0";
      begin
         if Long_Long_Float'Value (Str) /= 0.1 then
            raise Program_Error;
         end if;
      end;
   end loop;
end Float_Value1;
