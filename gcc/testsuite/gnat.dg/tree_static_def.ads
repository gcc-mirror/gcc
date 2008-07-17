package TREE_STATIC_Def is

   type Int is record
      Value : Integer;
   end record;

   procedure check (I : Int; v : integer);

   One : constant Int := (Value => 1);
end;
