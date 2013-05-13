! { dg-do compile }
! { dg-options "-fcoarray=single -fdump-tree-original" }
!
! PR fortran/57093
!
! Contributed by Damian Rouson
!
program main
  character(len=25), allocatable :: greeting[:]
  allocate(greeting[*])
  write(greeting,"(a)") "z"
end

! { dg-final { scan-tree-dump-times "greeting.data = \\(void . restrict\\) __builtin_malloc \\(25\\);" 1 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
