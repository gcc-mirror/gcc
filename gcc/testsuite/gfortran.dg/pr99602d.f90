! { dg-do compile }
! { dg-options "-fcheck=pointer -fdump-tree-original" }
!
! PR fortran/99602
!

module m
  implicit none
contains
  subroutine wr(y)
    class(*), pointer :: y
    if (associated (y)) stop 1
  end
end module m

use m
implicit none
class(*), pointer :: cptr

nullify (cptr)
call wr(cptr)
end

! { dg-final { scan-tree-dump-not "_gfortran_runtime_error_at" "original" } }
! { dg-final { scan-tree-dump-not "Pointer actual argument" "original" } }
