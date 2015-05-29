! { dg-do compile }
! { dg-options "-fdump-tree-original -fcoarray=lib" }
!
! Argument passing was wrong
!

program atomic
  use iso_fortran_env
  implicit none

  integer :: me
  integer(atomic_int_kind) :: atom[*]
  me = this_image()
  call atomic_define(atom[1],0)
  sync all
  call ATOMIC_ADD (atom[1], me)
  if(me == 1) call atomic_ref(me,atom[1])
  sync all
  write(*,*) me
end program

! { dg-final { scan-tree-dump-times "value.. = 0;" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_atomic_define \\(caf_token.0, 0, 1, &value.., 0B, 1, 4\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_atomic_op \\(1, caf_token.0, 0, 1, &me, 0B, 0B, 1, 4\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_atomic_ref \\(caf_token.0, 0, 1, &me, 0B, 1, 4\\);" 1 "original" } }
