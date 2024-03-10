! { dg-do run }
! { dg-additional-options "-fcheck=bounds -g -fdump-tree-original" }
! { dg-output "At line 13 .*" }
! { dg-shouldfail "Array bound mismatch for dimension 1 of array 'ivec' (2/3)" }
!
! PR fortran/31059 - runtime bounds-checking in presence of array constructors

program p
  integer              :: jvec(3) = [1,2,3]
  integer, allocatable :: ivec(:), kvec(:), lvec(:), mvec(:), nvec(:)
  ivec    = [1,2]   ! (re)allocation
  kvec    = [4,5,6] ! (re)allocation
  ivec(:) = [4,5,6] ! runtime error (->dump)
  ! not reached ...
  print *, jvec + [1,2,3] ! OK & no check generated
  print *, [4,5,6] + jvec ! OK & no check generated
  print *, lvec + [1,2,3] ! check generated (->dump)
  print *, [4,5,6] + mvec ! check generated (->dump)
  nvec(:) = jvec          ! check generated (->dump)
end

! { dg-final { scan-tree-dump-times "Array bound mismatch " 4 "original" } }
! { dg-final { scan-tree-dump-times "Array bound mismatch .*ivec" 1 "original" } }
! { dg-final { scan-tree-dump-times "Array bound mismatch .*lvec" 1 "original" } }
! { dg-final { scan-tree-dump-times "Array bound mismatch .*mvec" 1 "original" } }
! { dg-final { scan-tree-dump-times "Array bound mismatch .*nvec" 1 "original" } }
