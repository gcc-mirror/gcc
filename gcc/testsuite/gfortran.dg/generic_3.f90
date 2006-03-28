! { dg-do compile }
! Testcase from PR 17713
module fit_functions
  implicit none
contains
  subroutine gauss( x, a, y, dy, ma )
    double precision, intent(in)     :: x
    double precision, intent(in)     :: a(:)
    double precision, intent(out)    :: y
    double precision, intent(out)    :: dy(:)
    integer,          intent(in)     :: ma
  end subroutine gauss
end module fit_functions

subroutine mrqcof( x, y, sig, ndata, a, ia, ma )
  use fit_functions
  
  implicit none
  double precision, intent(in)   :: x(:), y(:), sig(:)
  integer,   intent(in)          :: ndata
  double precision, intent(in)   :: a(:)
  integer,   intent(in)          :: ia(:), ma
  
  integer                           i
  double precision                  yan, dyda(ma)
  
  do i = 1, ndata
     call gauss( x(i), a, yan, dyda, ma )
  end do
end subroutine mrqcof

! { dg-final { cleanup-modules "fit_functions" } }
