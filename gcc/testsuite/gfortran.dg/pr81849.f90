! { dg-do run }
! PR fortran/81849
program p
   implicit none
   integer  :: n=3
   if (any(g() /= f())) stop 1
   contains
      function g()
         real g(n)
         g = 7
      end function g
      function f() result(r)
         real r(n)
         r = 7
      end function f
end program
