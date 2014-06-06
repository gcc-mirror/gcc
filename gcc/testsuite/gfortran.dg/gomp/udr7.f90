! { dg-do compile }

module udr7m1
  type dt
    real :: r
  end type dt
end module udr7m1
module udr7m2
  use udr7m1
  interface operator(+)
    module procedure addm2
  end interface
!$omp declare reduction(+:dt:omp_out=omp_out+omp_in) &
!$omp & initializer(omp_priv=dt(0.0))
!$omp declare reduction(.myadd.:dt:omp_out=omp_out.myadd.omp_in) &
!$omp & initializer(omp_priv=dt(0.0))
  interface operator(.myadd.)
    module procedure addm2
  end interface
  private
  public :: operator(+), operator(.myadd.), dt
contains
  type(dt) function addm2 (x, y)
    type(dt), intent (in):: x, y
    addm2%r = x%r + y%r
  end function
end module udr7m2
module udr7m3
  use udr7m1
  private
  public :: operator(.myadd.), operator(+), dt
  interface operator(.myadd.)
    module procedure addm3
  end interface
!$omp declare reduction(+:dt:omp_out=omp_out+omp_in) &
!$omp & initializer(omp_priv=dt(0.0))
!$omp declare reduction(.myadd.:dt:omp_out=omp_out.myadd.omp_in) &
!$omp & initializer(omp_priv=dt(0.0))
  interface operator(+)
    module procedure addm3
  end interface
contains
  type(dt) function addm3 (x, y)
    type(dt), intent (in):: x, y
    addm3%r = x%r + y%r
  end function
end module udr7m3
module udr7m4
  use udr7m1
  private
  interface operator(.myadd.)
    module procedure addm4
  end interface
!$omp declare reduction(+:dt:omp_out=omp_out+omp_in) &
!$omp & initializer(omp_priv=dt(0.0))
!$omp declare reduction(.myadd.:dt:omp_out=omp_out.myadd.omp_in) &
!$omp & initializer(omp_priv=dt(0.0))
  interface operator(+)
    module procedure addm4
  end interface
contains
  type(dt) function addm4 (x, y)
    type(dt), intent (in):: x, y
    addm4%r = x%r + y%r
  end function
end module udr7m4
subroutine f1
  use udr7m2
  type(dt) :: d, e
  integer :: i
  d=dt(0.0)
  e = dt (0.0)
!$omp parallel do reduction (+ : d) reduction ( .myadd. : e)
  do i=1,100
    d=d+dt(i)
    e=e+dt(i)
  end do
end subroutine f1
subroutine f2
  use udr7m3	! { dg-error "Previous !.OMP DECLARE REDUCTION|Ambiguous interfaces" }
  use udr7m2	! { dg-error "Ambiguous !.OMP DECLARE REDUCTION" }
end subroutine f2
subroutine f3
  use udr7m4
  use udr7m2
end subroutine f3
subroutine f4
  use udr7m3
  use udr7m4
end subroutine f4
