! { dg-do run }
! Test the fix for pr22146, where and elemental subroutine with
! array actual arguments would cause an ICE in gfc_conv_function_call.
! This test checks that the main uses for elemental subroutines work
! correctly; namely, as module procedures and as procedures called
! from elemental functions. The compiler would ICE on the former with
! the first version of the patch.
!
! Contributed by Paul Thomas   <pault@gcc.gnu.org>

module type
  type itype
    integer :: i
    character(1) :: ch
  end type itype
end module type

module assign
  interface assignment (=)
    module procedure itype_to_int
  end interface
contains
  elemental subroutine itype_to_int (i, it)
    use type
    type(itype), intent(in) :: it
    integer, intent(out) :: i
    i = it%i
  end subroutine itype_to_int

  elemental function i_from_itype (it) result (i)
    use type
    type(itype), intent(in) :: it
    integer :: i
    i = it
  end function i_from_itype

end module assign

program test_assign
  use type
  use assign
  type(itype) :: x(2, 2)
  integer :: i(2, 2)

! Test an elemental subroutine call from an elementary function.
  x = reshape ((/(itype (j, "a"), j = 1,4)/), (/2,2/))
  forall (j = 1:2, k = 1:2)
    i(j, k) = i_from_itype (x (j, k))
  end forall
  if (any(reshape (i, (/4/)).ne.(/1,2,3,4/))) STOP 1

! Check the interface assignment (not part of the patch).
  x = reshape ((/(itype (j**2, "b"), j = 1,4)/), (/2,2/))
  i = x
  if (any(reshape (i, (/4/)).ne.(/1,4,9,16/))) STOP 2

! Use the interface assignment within a forall block.
  x = reshape ((/(itype (j**3, "c"), j = 1,4)/), (/2,2/))
  forall (j = 1:2, k = 1:2)
    i(j, k) = x (j, k)
  end forall
  if (any(reshape (i, (/4/)).ne.(/1,8,27,64/))) STOP 3

end program test_assign
