with Aggr20_Pkg; use Aggr20_Pkg;
with System;

package Aggr20 is

   type Rec1 is record
      Address : System.Address;
   end record;

   Nil_Rec1 : constant Rec1 := (Address => Default_Nil_Address);

   type Rec2 is record
      Callback : Rec1;
   end record;

   Nil_Rec2 : constant Rec2 := (Callback => Nil_Rec1);

   type Rec3 is record
      Callback : Rec2;
   end record;

   procedure Proc (R : out Rec3);

end Aggr20;
