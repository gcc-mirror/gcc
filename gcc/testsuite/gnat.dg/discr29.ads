-- { dg-do compile }

package Discr29 is

   type Rec1 is record
      I1 : Integer;
      I2 : Integer;
      I3 : Integer;
   end record;

   type Rec2 is tagged record
      I1 : Integer;
      I2 : Integer;
   end record;

   type Rec3 (D : Boolean) is record
      case D is
         when True =>  A : Rec1;
         when False => B : Rec2;
      end case;
   end record;

   procedure Proc (R : out Rec3);

   Tmp : Rec2;

end Discr29;
