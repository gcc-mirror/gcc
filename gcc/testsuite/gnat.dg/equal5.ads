package Equal5 is
   type Eq_Parent is tagged null record;

   function "="
     (Left  : Eq_Parent;
      Right : Eq_Parent) return Boolean;

   type Eq_Iface is interface;

   function "="
     (Left  : Eq_Iface;
      Right : Eq_Iface) return Boolean is abstract;
   procedure Op (Obj : Eq_Iface) is abstract;

   -----------------
   -- Derivations --
   -----------------

   type Child_6 is new Eq_Parent and Eq_Iface with null record;

   procedure Op (Obj : Child_6);

   function Equals
     (Left  : Child_6;
      Right : Child_6) return Boolean;

   function "="
     (Left  : Child_6;
      Right : Child_6) return Boolean renames Equals;  --  Test

end Equal5;
