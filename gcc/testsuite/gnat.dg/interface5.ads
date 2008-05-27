package interface5 is
   type B is tagged null record;
   
   type I is interface;
   function F (Object : I) return access I is abstract;
   
   type Child is new B and I with null record;
   function F (Object : Child) return access Child;
end interface5;
