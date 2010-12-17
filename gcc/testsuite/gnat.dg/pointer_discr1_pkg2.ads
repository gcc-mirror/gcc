with Unchecked_Conversion;
with Pointer_Discr1_Pkg3;

package Pointer_Discr1_Pkg2 is

  subtype T_WINDOW is Pointer_Discr1_Pkg3.T_WINDOW(Pointer_Discr1_Pkg3.One);

  function TO_WINDOW is new Unchecked_Conversion(Integer, T_WINDOW);

end Pointer_Discr1_Pkg2;
