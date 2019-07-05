--  { dg-do compile }

package body Bip_Export is
   function F return T is
   begin
      return Result : constant T := G do
         null;
      end return;
   end F;

   function G return T is
   begin
      return (null record);
   end G;
end Bip_Export;
