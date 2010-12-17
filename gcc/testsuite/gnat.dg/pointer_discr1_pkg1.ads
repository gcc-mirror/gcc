with Pointer_Discr1_Pkg2;

package Pointer_Discr1_Pkg1 is

  type Arr is array (1..4) of Pointer_Discr1_Pkg2.T_WINDOW;

  Window : Arr;

end Pointer_Discr1_Pkg1;
