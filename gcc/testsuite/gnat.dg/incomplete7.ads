package Incomplete7 is
   type Color;
   type Color is (red, green, blue);

   type Action (C : Color := Color'(red));
   type Action (C : Color := Color'(red)) is record
      case C is
         when red =>
            Stop_Time : Positive;

         when others =>
            Go_For_It : Integer;
      end case;
   end record;

   type Num;
   type Num is new Integer;

   type Rec (N : Num := Num'(1));
   type Rec (N : Num := Num'(1)) is record
      case N is
         when 1 =>
            One : Integer;

         when others =>
            null;
      end case;
   end record;

   procedure Foo;
end Incomplete7;
