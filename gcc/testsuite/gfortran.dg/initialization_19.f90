! { dg-do run }
! The following program fails with 4.3.0
! but works with 4.4.0. See:
!
! http://gcc.gnu.org/ml/fortran/2008-05/msg00199.html
!
module c
type d
  integer :: i=-1
end type d
end module c

module s
use c
contains
subroutine g
 type(d) :: a
 ! Without the following line it passes with 4.3.0:
 print *, a%i
 if(a%i /= -1) call abort()
 a%i=0
end subroutine g
end module s

program t
use c
use s

call g
call g

end program t

! ! { dg-final { cleanup-modules "c s" } }
