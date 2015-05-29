! { dg-do compile }
! { dg-options "-fdump-tree-original -fcoarray=lib" }
!
  real, allocatable :: values(:)[:]
  allocate(values(1024)[*])
  call  laplacian(values) 
contains
  subroutine laplacian(rhs) 
    real, allocatable :: rhs(:)[:]
    real :: local_laplacian(size(rhs))
    local_laplacian=0.
  end subroutine
end 

! { dg-final { scan-tree-dump-times "ubound.. = " 1 "original" } }
! { dg-final { scan-tree-dump-times "size.. = " 2 "original" } }

