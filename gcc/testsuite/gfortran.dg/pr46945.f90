! PR fortran/46945
! { dg-do run }
! { dg-options "-O -ftree-vrp -fno-tree-ccp -fno-tree-fre" }

program pr46945
  real, allocatable :: a(:,:,:)
  integer :: n
  n = 0
  allocate (a(n,n,n))
end program pr46945
