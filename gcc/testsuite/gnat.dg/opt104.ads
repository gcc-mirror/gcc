package Opt104 is

   type Time is record
      S  : Integer;
      B1 : Boolean;
      B2 : Boolean;
      B3 : Boolean;
      B4 : Boolean;
      B5 : Boolean;
      B6 : Boolean;
   end record;

   Zero_Time : constant Time :=
     (S  => 0,
      B1 => False,
      B2 => False,
      B3 => False,
      B4 => False,
      B5 => False,
      B6 => False);

   type Root is tagged null record;

   type Packed_Rec is record
      R : Root;
      B : Boolean;
      T : Time := Zero_Time;
   end record;
   pragma Pack (Packed_Rec);

   type Rec (D : Boolean) is record
      case D is
         when True  => Len : Integer;
         when False => null;
      end case;
   end record;

   procedure Proc (R : Rec);

end Opt104;
