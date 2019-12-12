package body Freezing1_Pack is
   function Create_Collection
     (Factory : in T_Factory) return I_Interface_Collection'Class
   is
   begin
      return Implem'(null record);
   end Create_Collection;
end Freezing1_Pack;
