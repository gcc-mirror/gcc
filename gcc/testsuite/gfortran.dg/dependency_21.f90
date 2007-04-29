! { dg-do run }
! Test the fix for PR31711 in which the dependency in the assignment
! at line 18 was detected and then ignored.
!
! Contributed by Tobias Ivarsson <thobes@gmail.com>
!
program laplsolv
  IMPLICIT NONE
  integer, parameter                  :: n = 2
  double precision,dimension(0:n+1, 0:n+1) :: T
  integer                             :: i

  T=0.0
  T(0:n+1 , 0)     = 1.0
  T(0:n+1 , n+1)   = 1.0
  T(n+1   , 0:n+1) = 2.0

  T(1:n,1)=(T(0:n-1,1)+T(1:n,1+1)+1d0)

  if (any (T(1:n,1) .ne. 1d0 )) call abort ()
end program laplsolv
