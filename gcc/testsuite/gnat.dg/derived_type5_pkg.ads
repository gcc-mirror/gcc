package Derived_Type5_Pkg is

   type T_Unsigned8  is new Natural range 0 .. (2 ** 8 - 1);

   type Rec (Discriminant : T_Unsigned8) is record
      Fixed_Field : T_Unsigned8;
      case Discriminant is
         when 0 =>
            Optional_Field : T_unsigned8;
         when others =>
            null;
      end case;
   end record;

   type Derived is new Rec (0);

   for Derived use record
      Fixed_Field    at 0 range 0  .. 7;
      Discriminant   at 0 range 8  .. 15;
      Optional_Field at 0 range 16 .. 23;
   end record;

   procedure Proc1 (R : in out Rec);

   procedure Proc2 (R : out Rec);

end Derived_Type5_Pkg;
