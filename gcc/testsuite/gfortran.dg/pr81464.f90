! { dg-do compile }
! { dg-options "--param parloops-chunk-size=2 -ftree-parallelize-loops=2 -O1" }

program main
  implicit none
  real, dimension(:,:),allocatable :: a, b, c
  real :: sm

  allocate (a(2,2), b(2,2), c(2,2))

  call random_number(a)
  call random_number(b)

  c = matmul(a,b)
  sm = sum(c)

  deallocate(a,b,c)

end program main
