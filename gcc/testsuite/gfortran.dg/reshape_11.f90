! { dg-do compile }
! { dg-options "-fmax-array-constructor=65536" }
! PR fortran/103794

program p
  integer :: i, j
  integer, parameter :: a(2) = 2, m = 20000
  integer, parameter :: e(*) = &
       [(reshape([1,2,3],  (a*i)), i=1,1)]     ! { dg-error "not enough elements" }
  integer, parameter :: g(*,*) = &
           reshape([([1,2,3,4],j=1,m)],[(a*i,i=1,1)]) ! { dg-error "number of elements" }
  print *, reshape([([1,2,3,4],j=1,m)],[(a*i,i=1,1)])
  print *,   reshape([1,2,3], [(a*i,  i=1,1)]) ! { dg-error "not enough elements" }
  print *, [(reshape([1,2,3],  (a*i)),i=1,1)]  ! { dg-error "not enough elements" }
end
