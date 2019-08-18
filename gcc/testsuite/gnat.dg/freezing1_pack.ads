package Freezing1_Pack is
   type T_Factory is abstract tagged private;
   type I_Interface_Collection is interface;

   Factory : constant T_Factory;

   function Create_Collection
     (Factory : in T_Factory) return I_Interface_Collection'Class;

   type Implem is new I_Interface_Collection with null record;

private
   type T_Factory is tagged null record;

   Factory : constant T_Factory := T_Factory'(null record);
end Freezing1_Pack;
