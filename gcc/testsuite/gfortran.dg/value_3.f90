! { dg-do compile }
! Tests the constraints in the patch for PR29642, which requested the
! implementation of the F2003 VALUE attribute for gfortran.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org> 
!
program test_value
  integer(8) :: i = 42, j   ! { dg-error "not a dummy" }
  integer(8), value :: k    ! { dg-error "not a dummy" }
  value :: j

contains
  subroutine bar_1 (i)
    integer(8) :: i
    dimension i(8)
    value :: i  ! { dg-error "conflicts with DIMENSION" }
    i = 0
  end subroutine bar_1

  subroutine bar_2 (i)
    integer(8) :: i
    pointer :: i
    value :: i  ! { dg-error "conflicts with POINTER" }
    i = 0
  end subroutine bar_2

  integer function bar_3 (i)
    integer(8) :: i
    dimension i(8)
    value :: bar_3  ! { dg-error "conflicts with FUNCTION" }
    i = 0
    bar_3 = 0
  end function bar_3

  subroutine bar_4 (i, j)
    integer(8), intent(inout) :: i
    integer(8), intent(out) :: j
    value :: i  ! { dg-error "conflicts with INTENT" }
    value :: j  ! { dg-error "conflicts with INTENT" }
    i = 0
    j = 0
  end subroutine bar_4

  integer function bar_5 ()
    integer(8) :: i
    external :: i
    integer, parameter :: j = 99
    value :: i  ! { dg-error "conflicts with EXTERNAL" }
    value :: j  ! { dg-error "PARAMETER attribute conflicts with" }
    bar_5 = 0
  end function bar_5

end program test_value
