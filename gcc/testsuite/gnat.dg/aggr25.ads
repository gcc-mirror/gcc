package Aggr25 is

  type T_A is (A, B , C ,D);

  subtype Has_B_D is T_A with Static_Predicate => Has_B_D in B | D;

  type Obj_T (Kind : T_A) is
    record
       case Kind is
        --OK-- when A | C => null; --OK--
        when Has_B_D  =>  Value : Boolean;
        --BAD-- when A | C => null;
        when others => null;
      end case;
    end record;

  type T is access Obj_T;

  Unavailable : constant T := new Obj_T'(Kind => A);

  procedure Foo;

end Aggr25;
