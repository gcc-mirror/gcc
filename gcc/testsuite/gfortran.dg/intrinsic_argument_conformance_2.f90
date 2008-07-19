! { dg-do compile }
! Some CSHIFT, EOSHIFT and UNPACK conformance tests
!
program main
  implicit none
  real, dimension(1)   :: a1, b1, c1
  real, dimension(1,1) :: a2, b2, c2
  real, dimension(1,0) :: a, b, c
  real :: tempn(1), tempv(5)
  real,allocatable :: foo(:)
  allocate(foo(0))
  tempn = 2.0

  a1 = 0
  a2 = 0
  c1 = 0
  a2 = 0

  b1 = cshift (a1,1)
  b1 = cshift (a1,(/1/)) ! { dg-error "must be a scalar" }
  b1 = eoshift (a1,1)
  b2 = eoshift (a1,c1(1)) ! { dg-error "must be INTEGER" }
  b1 = eoshift (a1,(/1/)) ! { dg-error "must be a scalar" }
  b1 = eoshift (a1,1,boundary=c1) ! { dg-error "must be a scalar" }
  b1 = eoshift (a1,(/1/), boundary=c2) ! { dg-error "must be a scalar" }

  b2 = cshift (a2,1)
  b2 = cshift (a2,(/1/))
  b2 = cshift (a2,reshape([1],[1,1])) ! { dg-error "have rank 1 or be a scalar" }
  b2 = eoshift (a2,1)
  b2 = eoshift (a2,c1) ! { dg-error "must be INTEGER" }
  b2 = eoshift (a2,(/1/))
  b2 = eoshift (a2,reshape([1],[1,1]), boundary=c1) ! { dg-error "have rank 1 or be a scalar" }
  b2 = eoshift (a2,1,boundary=c2(:,:)) ! { dg-error "have rank 1 or be a scalar" }
  b2 = eoshift (a2,(/1/), boundary=c2(:,:)) ! { dg-error "have rank 1 or be a scalar" }

  b = eoshift (a,(/1/), boundary=c(1,:)) ! { dg-error "Different shape in dimension 1" }

  if (any(eoshift(foo,dim=1,shift=1,boundary=(/42.0,-7.0/))/= 0)) call abort() ! { dg-error "must be a scalar" }
  if (any(eoshift(tempn(2:1),dim=1,shift=1,boundary=(/42.0,-7.0/))/= 0)) call abort() ! { dg-error "must be a scalar" }

  if (any(unpack(tempv,tempv(1:0)/=0,tempv) /= -47)) call abort() ! { dg-error "Different shape" }
  if (any(unpack(tempv(5:4),tempv(1:0)/=0,tempv) /= -47)) call abort() ! { dg-error "Different shape" }
end program main
