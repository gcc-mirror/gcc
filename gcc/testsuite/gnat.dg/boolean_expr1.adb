-- PR middle-end/36554
-- Origin: Laurent Guerby <laurent@guerby.net>

-- { dg-do compile }
-- { dg-options "-O2" }

package body Boolean_Expr1 is

   function Long_Float_Is_Valid (X : in Long_Float) return Boolean is
      Is_Nan : constant Boolean := X /= X;
      Is_P_Inf : constant Boolean := X > Long_Float'Last;
      Is_M_Inf : constant Boolean := X < Long_Float'First;
      Is_Invalid : constant Boolean := Is_Nan or Is_P_Inf or Is_M_Inf;
   begin
      return not Is_Invalid;
   end Long_Float_Is_Valid;

   function S (V : in Long_Float) return String is
   begin
      if not Long_Float_Is_Valid (V) then
         return "INVALID";
      else
         return "OK";
      end if;
   exception
      when others =>
         return "ERROR";
   end S;

end Boolean_Expr1;
