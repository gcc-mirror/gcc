package Pure_Function1 is
   function F return Integer;
   pragma Pure_Function (F);
   pragma Pure_Function (F);
   pragma Pure_Function (F);
end;
