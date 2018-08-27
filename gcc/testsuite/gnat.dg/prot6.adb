--  { dg-do compile }
--  { dg-options "-gnatc" }

package body Prot6 is

   protected body My_Type is

      procedure Set (D : Integer) is
      begin
         I := D;
      end Set;

      function Get return Integer is
      begin
         return I;
      end Get;
   end My_Type;

   procedure Dummy is null;
end Prot6;
