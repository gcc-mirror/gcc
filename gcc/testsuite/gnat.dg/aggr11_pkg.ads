package Aggr11_Pkg is

   type Error_Type is (No_Error, Error);

   type Rec (Kind : Error_Type := No_Error) is record
     case Kind is
       when Error => null;
       when others => B : Boolean;
     end case;
   end record;

   type Arr is array (1..6) of Rec;

end Aggr11_Pkg;
