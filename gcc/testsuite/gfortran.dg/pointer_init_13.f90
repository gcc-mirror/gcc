! { dg-do compile }
! { dg-options "-fcheck=pointer -fdump-tree-optimized -O -fno-inline" }
!
! PR fortran/48958
!
! Initialize non-saved pointers with -fcheck=pointer to support runtime checks
! of uses of possibly undefined pointers

program p
  implicit none
  call s
contains
  subroutine s
    integer, pointer :: a(:)
    integer, pointer :: b(:) => NULL()
    if (size (a) /= 0) stop 1
    if (size (b) /= 0) stop 2
    print *, size (a)
    print *, size (b)
  end
end

! { dg-final { scan-tree-dump-times "_gfortran_runtime_error_at" 1 "optimized" } }
! { dg-final { scan-tree-dump-not "_gfortran_stop_numeric" "optimized" } }
