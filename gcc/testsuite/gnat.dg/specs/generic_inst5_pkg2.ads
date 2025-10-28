with Generic_Inst5_Pkg1;

generic
   with package P1 is new Generic_Inst5_Pkg1 (<>);
   with package P1_N is new P1.Nested;
package Generic_Inst5_Pkg2 is
end Generic_Inst5_Pkg2;
