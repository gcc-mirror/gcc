! { dg-do compile } 
! { dg-require-effective-target vect_double } 

subroutine foo (x,nnd)
  dimension x(nnd) 
  integer i
  
  do i=1,nnd
    x(i) = 1.d0 + (1.d0*i)/nnd
  end do

end subroutine foo 

! { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_unpack  && vect_intfloat_cvt } } } } 

