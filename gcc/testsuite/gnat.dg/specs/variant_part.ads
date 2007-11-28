-- { dg-do compile }
package Variant_Part is
   type T1(b: boolean) is record
     case (b) is              -- { dg-error "discriminant name expected" }
        when others => null;
     end case;
   end record;
end Variant_Part;
