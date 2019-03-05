! { dg-do compile }
! PR fortran/88206
program p
   integer, parameter :: z(4) = [1,2,3,4]
   integer :: k = 2
   print *, [real(z(k))]
end

