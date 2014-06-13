! { dg-do compile }

module udr5m1
  type dt
    real :: r
  end type dt
end module udr5m1
module udr5m2
  use udr5m1
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
contains
  type(dt) function addm2 (x, y)
    type(dt), intent (in):: x, y
    addm2%r = x%r + y%r
  end function
end module udr5m2
module udr5m3
  use udr5m1
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
end module udr5m3
subroutine f1
  use udr5m2
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
  use udr5m3	! { dg-error "Previous !.OMP DECLARE REDUCTION|Ambiguous interfaces" }
  use udr5m2	! { dg-error "Ambiguous !.OMP DECLARE REDUCTION" }
end subroutine f2
