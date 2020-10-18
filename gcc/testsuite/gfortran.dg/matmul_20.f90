! { dg-do run }
! PR97063 - Wrong result for vector (step size is negative) * matrix

program p
  implicit none
  integer, parameter :: m = 3, k = 2*m, l = k-1, n = 4
  integer :: i, j,  m1, m2, ms
  integer :: ai(k), bi(k,n), ci(n), ci_ref(n), c1, c2
  real    :: ar(k), br(k,n), cr(n), cr_ref(n)

  ai(:)   = [(i,i=0,k-1)]
  bi(:,:) = reshape ([(((5*i+j),i=0,k-1),j=0,n-1)],[k,n])

  ! Parameters of subscript triplet
  m1 = 1; m2 = l; ms =  2

  ! Reference values for cross-checks: integer variant
  c1 = dot_product (ai(m1:m2: ms), bi(m1:m2: ms,1))
  c2 = dot_product (ai(m1:m2: ms), bi(m1:m2: ms,2))
  ci_ref = matmul  (ai(m1:m2: ms), bi(m1:m2: ms,:))
  ci     = matmul  (ai(m2:m1:-ms), bi(m2:m1:-ms,:))

  if (ci_ref(1) /= c1 .or. ci_ref(2) /= c2) stop 1
  if (any (ci   /= ci_ref)) stop 2

  ! Real variant
  ar = real (ai)
  br = real (bi)
  cr_ref = matmul  (ar(m1:m2: ms), br(m1:m2: ms,:))
  cr     = matmul  (ar(m2:m1:-ms), br(m2:m1:-ms,:))

  if (any (cr_ref /= real (ci_ref))) stop 3
  if (any (cr     /=       cr_ref )) stop 4

  ! Mixed variants
  cr_ref = matmul  (ar(m1:m2: ms), bi(m1:m2: ms,:))
  cr     = matmul  (ar(m2:m1:-ms), bi(m2:m1:-ms,:))

  if (any (cr_ref /= real (ci_ref))) stop 5
  if (any (cr     /=       cr_ref )) stop 6

  cr_ref = matmul  (ai(m1:m2: ms), br(m1:m2: ms,:))
  cr     = matmul  (ai(m2:m1:-ms), br(m2:m1:-ms,:))

  if (any (cr_ref /= real (ci_ref))) stop 7
  if (any (cr     /=       cr_ref )) stop 8
end program
