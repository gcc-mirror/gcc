-- { dg-do compile }

with Pack20_Pkg; use Pack20_Pkg;

package Pack20 is

   type Rec is record
      Simple_Type  : Integer;
      Fixed        : String_Ptr;
   end record;
   pragma Pack (Rec);

   procedure Proc (A : Rec);

end Pack20;
