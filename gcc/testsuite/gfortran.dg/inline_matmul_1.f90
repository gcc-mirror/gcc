! { dg-do  run }
! { dg-options "-ffrontend-optimize -fdump-tree-optimized -Wrealloc-lhs" }
! PR 37131 - check basic functionality of inlined matmul, making
! sure that the library is not called, with and without reallocation.

program main
  implicit none
  integer, parameter :: offset = -2
  real, dimension(3,2) :: a
  real, dimension(2,4) :: b
  real, dimension(3,4) :: c
  real, dimension(3,4) :: cres
  real, dimension(:,:), allocatable :: c_alloc
  integer, parameter :: a1_lower_p = 1 + offset, a1_upper_p = size(a,1) + offset
  integer, parameter :: a2_lower_p = 1 + offset, a2_upper_p = size(a,2) + offset
  integer, parameter :: b1_lower_p = 1 + offset, b1_upper_p = size(b,1) + offset
  integer, parameter :: b2_lower_p = 1 + offset, b2_upper_p = size(b,2) + offset
  integer, parameter :: c1_lower_p = 1 + offset, c1_upper_p = size(c,1) + offset
  integer, parameter :: c2_lower_p = 1 + offset, c2_upper_p = size(c,2) + offset
  real, dimension(a1_lower_p:a1_upper_p, a2_lower_p:a2_upper_p) :: ap
  real, dimension(b1_lower_p:b1_upper_p, b2_lower_p:b2_upper_p) :: bp
  real, dimension(c1_lower_p:c1_upper_p, c2_lower_p:c2_upper_p) :: cp
  real, dimension(4,8,4) :: f, fresult
  integer :: eight = 8, two = 2

  type foo
     real :: a
     integer :: i
  end type foo

  type(foo), dimension(3,2) :: afoo
  type(foo), dimension(2,4) :: bfoo
  type(foo), dimension(3,4) :: cfoo

  data a / 2.,  -3.,  5.,  -7., 11., -13./
  data b /17., -23., 29., -31., 37., -39., 41., -47./
  data cres /195., -304.,  384.,  275., -428.,  548.,  347., -540.,  692.,  411., -640.,  816./
  data fresult / &
   0.,   0., 195.,   0.,   0.,  17.,   0.,   0.,   0., -23.,-304.,   0.,   0.,   0.,   0.,   0., &
   0.,   0., 384.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0., &
   2.,   0., 275.,   0.,  -3.,  29.,   0.,   0.,   5., -31.,-428.,   0.,   0.,   0.,   0.,   0., &
   0.,   0., 548.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0., &
  -7.,   0., 347.,   0.,  11.,  37.,   0.,   0., -13., -39.,-540.,   0.,   0.,   0.,   0.,   0., &
   0.,   0., 692.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0., &
   0.,   0., 411.,   0.,   0.,  41.,   0.,   0.,   0., -47.,-640.,   0.,   0.,   0.,   0.,   0., &
   0.,   0., 816.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0./

  integer :: a1 = size(a,1), a2 = size(a,2)
  integer :: b1 = size(b,1), b2 = size(b,2)
  integer :: c1 = size(c,1), c2 = size(c,2)

  integer :: a1_lower, a1_upper, a2_lower, a2_upper
  integer :: b1_lower, b1_upper, b2_lower, b2_upper
  integer :: c1_lower, c1_upper, c2_lower, c2_upper

  a1_lower = 1 + offset ; a1_upper = a1 + offset
  a2_lower = 1 + offset ; a2_upper = a2 + offset
  b1_lower = 1 + offset ; b1_upper = b1 + offset
  b2_lower = 1 + offset ; b2_upper = b2 + offset
  c1_lower = 1 + offset ; c1_upper = c1 + offset
  c2_lower = 1 + offset ; c2_upper = c2 + offset

  c = matmul(a,b)
  if (sum(abs(c-cres))>1e-4) call abort

  c_alloc = matmul(a,b)      ! { dg-warning "Code for reallocating the allocatable array" }
  if (sum(abs(c_alloc-cres))>1e-4) call abort
  if (any([size(c_alloc,1), size(c_alloc,2)] /= [3,4])) call abort
  deallocate(c_alloc)

  allocate(c_alloc(4,4))
  c_alloc = matmul(a,b)      ! { dg-warning "Code for reallocating the allocatable array" }
  if (sum(abs(c_alloc-cres))>1e-4) call abort
  if (any([size(c_alloc,1), size(c_alloc,2)] /= [3,4])) call abort
  deallocate(c_alloc)

  allocate(c_alloc(3,3))
  c_alloc = matmul(a,b)      ! { dg-warning "Code for reallocating the allocatable array" }
  if (sum(abs(c_alloc-cres))>1e-4) call abort
  if (any([size(c_alloc,1), size(c_alloc,2)] /= [3,4])) call abort

  c_alloc = 42.
  c_alloc(:,:) = matmul(a,b)
  if (sum(abs(c_alloc-cres))>1e-4) call abort
  if (any([size(c_alloc,1), size(c_alloc,2)] /= [3,4])) call abort

  deallocate(c_alloc)
  
  ap = a
  bp = b
  cp = matmul(ap, bp)
  if (sum(abs(cp-cres)) > 1e-4) call abort

  f = 0
  f(1,1:3,2:3) = a
  f(2,2:3,:) = b
  c = matmul(f(1,1:3,2:3), f(2,2:3,:))
  if (sum(abs(c-cres))>1e-4) call abort

  f(3,1:eight:2,:) = matmul(a, b)
  if (sum(abs(f(3,1:eight:2,:)-cres))>1e-4) call abort

  afoo%a = a
  bfoo%a = b
  cfoo%a = matmul(afoo%a, bfoo%a)

  if (sum(abs(cfoo%a-cres)) > 1e-4) call abort

  block
    real :: aa(a1, a2), bb(b1, b2), cc(c1, c2)
    real :: am(a1_lower:a1_upper, a2_lower:a2_upper)
    real :: bm(b1_lower:b1_upper, b2_lower:b2_upper)
    real :: cm(c1_lower:c1_upper, c2_lower:c2_upper)

    aa = a
    bb = b
    am = a
    bm = b

    cc = matmul(aa,bb)
    if (sum(cc-cres)>1e-4) call abort
    c_alloc = matmul(aa,bb)    ! { dg-warning "Code for reallocating the allocatable array" }
    if (sum(abs(c_alloc-cres))>1e-4) call abort
    if (any([size(c_alloc,1), size(c_alloc,2)] /= [3,4])) call abort
    c_alloc = 42.
    deallocate(c_alloc)

    allocate(c_alloc(4,4))
    c_alloc = matmul(aa,bb)   ! { dg-warning "Code for reallocating the allocatable array" }
    if (sum(abs(c_alloc-cres))>1e-4) call abort
    if (any([size(c_alloc,1), size(c_alloc,2)] /= [3,4])) call abort
    deallocate(c_alloc)

    allocate(c_alloc(3,3))
    c_alloc = matmul(aa,bb)  ! { dg-warning "Code for reallocating the allocatable array" }
    if (sum(abs(c_alloc-cres))>1e-4) call abort
    if (any([size(c_alloc,1), size(c_alloc,2)] /= [3,4])) call abort
    deallocate(c_alloc)

    cm = matmul(am, bm)
    if (sum(abs(cm-cres)) > 1e-4) call abort

    cm = 42.

    cm(:,:) = matmul(a,bm)
    if (sum(abs(cm-cres)) > 1e-4) call abort

  end block

end program main

! { dg-final { scan-tree-dump-times "_gfortran_matmul" 0 "optimized" } }
