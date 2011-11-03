-- { dg-do compile }
-- { dg-options "-O3" }
-- { dg-options "-O3 -msse2" { target i?86-*-* x86_64-*-* } }

package body Loop_Optimization10 is

   function F (Low, High : in Array_Real_Type) return Array_Limit_Type is
      Result : Array_Limit_Type;
   begin
      for I in Result'Range
      loop
         Result (I) := F (Low (I), High (I));
      end loop;
      return Result;
   end;

end Loop_Optimization10;

