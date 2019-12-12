--  { dg-do compile }
--  { dg-options "-gnatwa" }

with Text_IO; use Text_IO;

package body Warn29 is
   procedure P (X : T; Y : Integer) is
   begin
      Put_Line ("hello");
   end P;
end Warn29;
