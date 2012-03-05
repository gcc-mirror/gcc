-- { dg-do compile }

package body Discr35 is

   procedure Proc1 is
     R : Rec2 := Null_Rec2;
   begin
     null;
   end;

   procedure Proc2 is
     R : Rec2;
   begin
     R := Null_Rec2;
   end;

end Discr35;
