-- { dg-do compile }
package Access_Constant_Decl is

   c: aliased constant integer := 3;

   type const_ptr is access constant integer;
   cp : const_ptr := c'access;

   x : access integer := cp; -- { dg-error "access-to-constant" }

end Access_Constant_Decl;
