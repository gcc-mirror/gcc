-- { dg-do compile }
-- { dg-options "-O" }

package Opt4 is

   type Rec (D : Boolean := False) is record
      case D is
         when False => null;
         when True => I : Integer;
      end case;
   end record;

   Null_Rec : constant Rec := (D => False);

   type I1 is limited interface;

   type I2 is limited interface;

   function Func (Data : I2) return Rec is abstract;

   type Ext is limited new I1 and I2 with null record;

   overriding function Func (Data : Ext) return Rec is (Null_Rec);

end Opt4;
