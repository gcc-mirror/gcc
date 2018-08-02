package Abstract_State1
  with Abstract_State => null,
       Initializes    => null
is
   type Complex (B : Boolean) is tagged private;
   type No_F is tagged private;
   X : constant No_F;

   procedure Foo;

private
   type Complex (B : Boolean) is tagged record
      G : Integer;
      case B is
         when True =>
            F : Integer;
         when False =>
            null;
      end case;
   end record;

   type No_F is new Complex (False) with null record;
   X : constant No_F := (B => False, G => 7);
end Abstract_State1;
