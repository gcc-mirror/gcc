-- { dg-do compile }
package Integer_Value is
   X : constant Integer :=
     Integer'Integer_Value (12.8); -- { dg-error "fixed-point type" }
end Integer_Value;
