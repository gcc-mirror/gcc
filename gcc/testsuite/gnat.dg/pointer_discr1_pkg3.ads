package Pointer_Discr1_Pkg3 is

  type T_TYPE is (One, Two, Three);

  type T_DATA (D : T_TYPE);

  type T_DATA (D : T_TYPE) is null record;

  type T_WINDOW is access T_DATA;

  procedure Map (Window : in T_WINDOW);

end Pointer_Discr1_Pkg3;
