package Debug6_Pkg is

   type Vkind is (Int, Undefined);
   for Vkind use (Int => -2 ** 31, Undefined => 0);

    type Value (Kind : Vkind) is record
       case Kind is
          when Undefined => null;
          when Int       => Value : Integer;
          when others    => null;
       end case;
    end record;

    procedure Process (V : Value);

end Debug6_Pkg;
