! { dg-do run }
!
! Rejected valid
!
! ! Contributed by Janus Weil  <janus@gcc.gnu.org>
!
implicit none

type :: param_matrix(k,c,r)
  integer, kind :: k
  integer, len :: c,r
  real(kind=k) :: m(c,r)
end type

type(param_matrix(8,3,2)) :: mat
real(kind=mat%k) :: m    ! Corrected error: Parameter ‘mat’ at (1) has not been declared or ...

if (kind(m) .ne. 8) STOP 1

end
