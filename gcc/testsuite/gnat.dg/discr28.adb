-- { dg-do compile }

package body Discr28 is

   procedure Dummy (E : Rec) is
   begin
     null;
   end;

   function F return Rec is
   begin
      return Default_Rec;
   end;

   procedure Proc1 is
   begin
      Dummy (F);
   end;

   procedure Proc2 is
   begin
      Dummy (F);
   end;

end Discr28;
