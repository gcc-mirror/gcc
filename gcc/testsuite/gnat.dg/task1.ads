with Task1_Pkg; use Task1_Pkg;

package Task1 is
   TAB : constant Typ_Task_Par_Tab := (others => (Dummy => FALSE));

   T1 : Typ_Task (TAB (1).Dummy);
   T2 : Typ_Task (TAB (2).Dummy);

   procedure Dummy;
end Task1;
