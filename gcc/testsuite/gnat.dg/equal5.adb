--  { dg-do compile }

package body Equal5 is
   function "="
     (Left  : Eq_Parent;
      Right : Eq_Parent) return Boolean is (True);

   procedure Op (Obj : Child_6) is null;

   function Equals
     (Left  : Child_6;
      Right : Child_6) return Boolean is (True);
end Equal5;
