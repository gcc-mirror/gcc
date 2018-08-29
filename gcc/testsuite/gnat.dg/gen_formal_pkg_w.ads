with Gen_Formal_Pkg_A, Gen_Formal_Pkg_B;

generic
   with package Ai is new Gen_Formal_Pkg_A (<>);
package Gen_Formal_Pkg_W is

   procedure P1 (T : Ai.T1) is null;

   package Bi is new Gen_Formal_Pkg_B (Ai.T1, Ai);

   procedure P2 (T : Ai.T1) is null;

end Gen_Formal_Pkg_W;
