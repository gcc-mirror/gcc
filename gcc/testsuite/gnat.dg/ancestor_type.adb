-- { dg-do compile }

package body Ancestor_Type is

   package body B is
      function make return T is
      begin
         return (T with n => 0);  -- { dg-error "expect ancestor" }
      end make;

   end B;

end Ancestor_Type;
