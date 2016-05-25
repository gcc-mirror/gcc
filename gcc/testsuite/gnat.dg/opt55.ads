package Opt55 is

   type Date is record
      D : Float;
   end record;

   type Rec1 (Kind : Boolean := False) is record
      case Kind is
         when True => N : Natural;
         when False => null;
      end case;
   end record;

   type Rec2 (D : Positive) is record
      R  : Rec1;
      D1 : Date;
      D2 : Date;
   end record;

   function F (C : Rec2; B : Boolean) return Date;

end Opt55;
