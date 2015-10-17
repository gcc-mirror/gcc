! { dg-do compile }
! PR fortran/67987
! PR fortran/67988
! Original code contributed by Gerhard Steinmetz
! gerhard dot steinmetz dot fortran at t-online dot de
subroutine p
   character(-8) :: c = ' '
end subroutine p

subroutine pp
   character(3), parameter :: c = 'abc'
   character(3) :: x(1)
   x = c(:-2)
   print *, len(trim(x(1)))
   x = [ c(:-2) ]
   print *, len(trim(x(1)))
end subroutine pp

