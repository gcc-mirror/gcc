package Ancestor_Type is

   type T is tagged private;

   package B is
      function make return T;
   end B;

private
   type T is tagged record
      n: Natural;
   end record;
end Ancestor_Type;
