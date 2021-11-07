! { dg-do compile }
! Original code from Gerhard Steinmetz
! Gerhard dot Steinmetz for fortran at t-online dot de
! PR fortran/68019
!
program p
   integer :: i
   type t
      integer :: n
   end type
   type(t), parameter :: vec(*) = [(t(i), i = 1, 4)]
   type(t), parameter :: arr(*) = reshape(vec, [2, 2])   ! { dg-error "Rank mismatch" }
end
