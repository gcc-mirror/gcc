generic
package Generic_Inst16_Pkg.Child.Grandchild is

   type CT is new GPT with private;

   Zippo_CT1 : constant CT;
   Zippo_CT2 : constant CT;

private

   type CT is new PT with
      record
         Small_Pi : Natural := 314;
      end record;

   Zippo_CT1 : constant CT := (Zippo_PT with Small_Pi => 0);
   Zippo_CT2 : constant CT :=
     (Generic_Inst16_Pkg.Child.Zippo_PT with Small_Pi => 0);

end Generic_Inst16_Pkg.Child.Grandchild;
