! { dg-do compile }
! Tests the fix for PR32464, where the use associated procedure would
! mess up the check for "grandparent" host association.
!
! Contributed by Harald Anlauf <anlauf@gmx.de>
!

module gfcbug64_mod1
  implicit none

  public :: inverse

  interface inverse
     module procedure copy
  end interface

contains

  function copy (d) result (y)
    real, intent(in) :: d(:)
    real             :: y(size (d))     ! <- this version kills gfortran
!    real, intent(in) :: d
!    real             :: y
    y = d
  end function copy

end module gfcbug64_mod1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module gfcbug64_mod2
  implicit none
contains

  subroutine foo (x_o)
    real, intent(in) :: x_o(:)

    integer          :: s(size (x_o))           ! <- this line kills gfortran

  contains

    subroutine bar ()
      use gfcbug64_mod1, only: inverse          ! <- this line kills gfortran
    end subroutine bar

  end subroutine foo
end module gfcbug64_mod2
! { dg-final { cleanup-modules "gfcbug64_mod1 gfcbug64_mod2" } }
