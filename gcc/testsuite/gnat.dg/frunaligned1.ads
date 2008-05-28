package FRUnaligned1 is
   type r is array (1 .. 72) of Boolean;
   pragma Pack (r);
   type s is record
      x : Boolean;
      y : r;
   end record;
   for s use record
      x at 0 range 0 .. 0;
      y at 0 range 1 .. 72;
   end record;
end FRUnaligned1;
