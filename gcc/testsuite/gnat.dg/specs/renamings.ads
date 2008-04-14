package Renamings is

   package Inner is
      procedure PI (X : Integer);
   end Inner;

   procedure P (X : Integer) renames Inner.PI;
   procedure P (X : Float);
   pragma Convention (C, P); -- { dg-error "non-local renaming" }

   procedure Q (X : Float);
   procedure Q (X : Integer) renames Inner.PI;
   pragma Convention (C, Q); -- { dg-error "non-local renaming" }
end Renamings;
