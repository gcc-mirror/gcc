! { dg-do compile }
! { dg-options "-fno-inline-arg-packing -O -fdump-tree-original" }
! PR fortran/92738, middle-end/91512
! Check that -fno-inline-pack does indeed suppress inline packing.
module x
  implicit none
contains
  subroutine foo(x)
    real, dimension(:), intent(inout) :: x
    call bar (x, size(x))
  end subroutine foo
  subroutine bar (x, n)
    integer, intent(in) :: n
    real, dimension(n) :: x
    x = -x
  end subroutine bar
end module x
! { dg-final { scan-tree-dump-times "_gfortran_internal_pack" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_internal_unpack" 1 "original" } }
