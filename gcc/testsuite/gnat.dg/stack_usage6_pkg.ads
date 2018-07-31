package Stack_Usage6_Pkg is

   type Rec (D : Boolean := False) is record
      case D is
         when False =>
            Foo : Integer;
            Bar : Integer;
         when True =>
            null;
      end case;
   end record;

   type Index_Type is new Integer range 0 .. 5;

   type Arr is array (Index_Type) of Rec;

   A : Arr;
   
end Stack_Usage6_Pkg;
