with Inline18_Gen1.Inner_G;

package Inline18_Pkg2.Child is

  package Base is new Inline18_Gen1 (Integer);

  package General is new Base.Inner_G;

end Inline18_Pkg2.Child;
