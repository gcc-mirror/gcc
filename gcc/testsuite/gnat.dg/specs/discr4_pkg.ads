package Disc4_Pkg is

   type Enum is (One, Two, Three);

   type Rec is private;

   Rec_One : constant Rec;
   Rec_Three  : constant Rec;

   function Get (Value : Integer) return Rec;

private

   type Rec (D : Enum := Two) is record
      case D is
         when One => null;
         when Two => Value : Integer;
         when Three => null;
      end case;
   end record;

   Rec_One   : constant Rec := (D => One);
   Rec_Three : constant Rec := (D => Three);

   function Get (Value : Integer) return Rec is (Two, Value);

end Disc4_Pkg;
