! { dg-do run }
! Tests fix for PR60717 in which offsets in recursive calls below
! were not being set correctly.
!
! Reported on comp.lang.fortran by Thomas Schnurrenberger
!
module m
  implicit none
  real :: chksum0 = 0, chksum1 = 0, chksum2 = 0
contains
  recursive subroutine show_real(a)
    real, intent(in) :: a(:)
    if (size (a) > 0) then
      chksum0 = a(1) + chksum0
      call show_real (a(2:))
    end if
    return
  end subroutine show_real
  recursive subroutine show_generic1(a)
    class(*), intent(in) :: a(:)
    if (size (a) > 0) then
      select type (a)
      type is (real)
        chksum1 = a(1) + chksum1
      end select
      call show_generic1 (a(2:)) ! recursive call outside SELECT TYPE
    end if
    return
  end subroutine show_generic1
  recursive subroutine show_generic2(a)
    class(*), intent(in) :: a(:)
    if (size (a) > 0) then
      select type (a)
      type is (real)
        chksum2 = a(1) + chksum2
        call show_generic2 (a(2:)) ! recursive call inside SELECT TYPE
      end select
    end if
    return
  end subroutine show_generic2
end module m
program test
  use :: m
  implicit none
  real :: array(1:6) = (/ 0, 1, 2, 3, 4, 5 /)
  call show_real (array)
  call show_generic1 (array)
  call show_generic2 (array)
  if (chksum0 .ne. chksum1) STOP 1
  if (chksum0 .ne. chksum2) STOP 2
end program test
