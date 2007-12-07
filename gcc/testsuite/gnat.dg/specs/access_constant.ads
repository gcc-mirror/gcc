-- { dg-do compile }
package Access_Constant is

   c: aliased constant integer := 3;

   type const_ptr is access constant integer;
   cp : const_ptr := c'access;

   procedure inc (var_ptr: access integer :=
     cp)  -- { dg-error "access-to-constant" }
      is abstract;

end Access_Constant;
