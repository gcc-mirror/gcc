package Type_Conv2 is

   type Root is abstract tagged limited null record;

   type Der_I is new Root with record
      X : Integer;
   end record;

   function Wrap (X : Integer) return Root'Class;

   procedure Proc_Static;

end Type_Conv2;
