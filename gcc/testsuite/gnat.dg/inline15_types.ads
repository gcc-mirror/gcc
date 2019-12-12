package Inline15_Types is
   type Enum is (One, Two, Three, Four);

   type Rec (Discr : Enum) is record
      Comp_1 : Integer;

      case Discr is
         when One =>
            Comp_2 : Float;
         when Two =>
            Comp_3 : Boolean;
            Comp_4 : Long_Float;
         when others =>
            null;
      end case;
   end record;
end Inline15_Types;
