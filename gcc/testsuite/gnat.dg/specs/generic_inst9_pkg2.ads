with Generic_Inst9_Pkg1;

generic
   type T2 is private;
package Generic_Inst9_Pkg2 is
   package the_pak1 is new Generic_Inst9_Pkg1 (T1 => T2);
end Generic_Inst9_Pkg2;
