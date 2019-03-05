--  { dg-do compile }
--  { dg-options "-O2 -Wuninitialized" }

package body Warn19 is

   procedure P_Init (X : out Bits) is
   begin
      Init (X);
   end;
   
   procedure Setup is
      Data : Bits;
   begin
      P_Init (Data);
      for I in Data'Range loop
         P_Data (I) := Data (I);
      end loop;
   end;

end Warn19;
