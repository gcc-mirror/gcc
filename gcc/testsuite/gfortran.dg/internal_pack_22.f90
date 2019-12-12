! { dg-do run }
! { dg-additional-options "-fdump-tree-original -O" }
! Check that absent and present dummy arguments work with
! packing when handing them down to an old-fashioned argument.

module x
  implicit none
contains
  subroutine foo (a,b)
    real, dimension(:), intent(inout), optional :: a, b
    if (present(a)) stop 1
    if (.not. present(b)) stop 2
    call bar (a, b)
  end subroutine foo

  subroutine bar (a,b)
    real, dimension(2), intent(inout), optional :: a, b
    real :: tmp
    if (present(a)) stop 3
    if (.not. present(b)) stop 4
    tmp = b(2)
    b(2) = b(1)
    b(1) = tmp
  end subroutine bar
end module x

program main
  use x
  implicit none
  real, dimension(2) :: b
  b(1) = 1.
  b(2) = 42.
  call foo(b=b)
  if (b(1) /= 42. .or. b(2)  /= 1.) stop 5
end program main
! { dg-final { scan-tree-dump-not "_gfortran_internal_unpack" "original" } }
