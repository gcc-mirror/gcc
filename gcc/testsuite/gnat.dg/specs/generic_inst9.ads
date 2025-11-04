-- { dg-do compile }

with Generic_Inst9_Pkg1;
with Generic_Inst9_Pkg2.G;

package Generic_Inst9 is

   type T4 is null record;
   type T5 is null record;

   subtype T3 is T5;

   type T4_ptr is access T4;
   type T5_ptr is access T5;

   package My_Pkg2 is new Generic_Inst9_Pkg2 (T2 => T4);
   package My_G4 is new My_Pkg2.G (T4_ptr); -- { dg-bogus "does not match|abandoned" }
   package My_G5 is new My_Pkg2.G (T5_ptr); -- { dg-error "does not match|abandoned" }

end Generic_Inst9;
