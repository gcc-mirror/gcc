generic
package Generic_Inst16_Pkg.Child is

   type PT is new GPT with private;

   Zippo_PT : constant PT;
   
private
   
   type PT is new GPT with
      record
         Pos_Pi : Natural := 314159265;
      end record;

   Zippo_PT : constant PT := (Pos_Pi => 0);

end Generic_Inst16_Pkg.Child;
