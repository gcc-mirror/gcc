integer function f()
   f(g) = 0       ! { dg-error "Statement function" }
contains
   integer function g()
   end
end
