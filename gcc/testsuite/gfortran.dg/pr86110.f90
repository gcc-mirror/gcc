! { dg-do compile }
! PR fortran/86110
program p
   character(:), allocatable :: x, y
   x = 'abc'
   y = [x(:)]  ! { dg-error "Incompatible ranks 0 and 1" }
end
