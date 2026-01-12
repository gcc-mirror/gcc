! { dg-do compile }
function f()
   f(g) = 0       ! { dg-error "Statement function" }
contains
   function g()
   end
end

