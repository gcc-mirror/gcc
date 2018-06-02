--  { dg-do compile }

package body Discr53 is

   function F return Rec is
      Data : Rec;
   begin
      return Data;
   end;

   type Ptr is access Rec;

   procedure Proc is
      Local : Ptr;
   begin
      Local := new Rec'(F);
   end;

end Discr53;
