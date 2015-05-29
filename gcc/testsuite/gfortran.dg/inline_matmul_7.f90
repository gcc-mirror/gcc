! { dg-do  run }
! { dg-options "-ffrontend-optimize -fdump-tree-original" }

program main
  implicit none
  real(kind=8), ALLOCATABLE :: a(:,:), b(:,:), v1(:), v2(:)
  real(kind=8), dimension(3,3) :: v1res, v2res
  integer :: n, i

  data v1res/ 442.d0,   -492.d0,   586.d0, &
            -4834.d0,   5694.d0, -7066.d0, &
            13042.d0, -15450.d0, 19306.d0 /

  data v2res/ 5522.d0,  -6310.d0,   7754.d0, &
             -7794.d0,   8982.d0, -11034.d0, &
             10490.d0, -12160.d0,  14954.d0 /
  n = 3

  ALLOCATE(a(N,N),b(N,N),v1(N), v2(N))

  a = reshape([((-1)**i*(-i-5)*(i+3)+5,i=1,n**2)], shape(a))
  b = reshape([((-1)**i*(-i-1)*(i-2),i=1,n**2)], shape(a))

  DO i=1,N
     v1 = MATMUL(a,b(:,i))
     if (any(abs(v1-v1res(:,i)) > 1e-10)) call abort

     v2 = MATMUL(a,b(i,:))
     if (any(abs(v2-v2res(:,i)) > 1e-10)) call abort

  ENDDO

END program main
! { dg-final { scan-tree-dump-times "_gfortran_matmul" 0 "original" } }
