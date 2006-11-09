! { dg-do compile }
! Fix for PR21730 - declarations used to produce the error:
!   target        :: x                ! these 2 lines interchanged
!                    1
! Error: Cannot change attributes of symbol at (1) after it has been used.
!
! Contributed by Harald Anlauf <anlauf@gmx.de>
!
subroutine gfcbug27 (x)
  real, intent(inout) :: x(:)

  real          :: tmp(size (x,1))  ! gfc produces an error unless
  target        :: x                ! these 2 lines interchanged
  real, pointer :: p(:)

  p => x(:)
end subroutine gfcbug27
