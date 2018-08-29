with Gen_Formal_Pkg_A;

generic
   type T1 is private;
   with package Ai is new Gen_Formal_Pkg_A (T1);
package Gen_Formal_Pkg_B is end;
