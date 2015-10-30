! { dg-do compile }
! PR fortran/68154
! Original code contributed by Gerhard Steinmetz
! gerhard dot steinmetz dot fortran at t-online dot de
program p
   character(1), parameter :: x1(2) = 'a'
   character(*), parameter :: x2(2) = x1
   character(*), parameter :: x3(*) = x1
end
