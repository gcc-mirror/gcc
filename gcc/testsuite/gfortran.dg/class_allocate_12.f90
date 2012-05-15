! { dg-do run }
!
! PR fortran/51972
!
! Contributed by Damian Rouson
!
module surrogate_module
  type ,abstract :: surrogate
  end type
end module

module strategy_module
  use surrogate_module

  type :: strategy
  end type
end module

module integrand_module
  use surrogate_module
  use strategy_module
  implicit none

  type ,abstract, extends(surrogate) :: integrand
    class(strategy), allocatable :: quadrature  
  end type
end module integrand_module

module lorenz_module
  use strategy_module
  use integrand_module
  implicit none

  type ,extends(integrand) :: lorenz
    real, dimension(:), allocatable :: state
  contains
    procedure ,public :: assign   => assign_lorenz
  end type
contains
  type(lorenz) function constructor(initial_state, this_strategy)
    real ,dimension(:) ,intent(in)  :: initial_state
    class(strategy)    ,intent(in)  :: this_strategy
    constructor%state=initial_state
    allocate (constructor%quadrature, source=this_strategy)
  end function

  subroutine assign_lorenz(lhs,rhs)
    class(lorenz)    ,intent(inout) :: lhs
    class(integrand) ,intent(in)    :: rhs
    select type(rhs)
      class is (lorenz)
        allocate (lhs%quadrature, source=rhs%quadrature)
        lhs%state=rhs%state
    end select
  end subroutine
end module lorenz_module

module runge_kutta_2nd_module 
  use surrogate_module,only : surrogate
  use strategy_module ,only : strategy
  use integrand_module,only : integrand
  implicit none

  type, extends(strategy) ,public :: runge_kutta_2nd
  contains
    procedure, nopass :: integrate
  end type
contains
  subroutine integrate(this)
    class(surrogate) ,intent(inout) :: this
    class(integrand) ,allocatable   :: this_half

    select type (this)
      class is (integrand)
        allocate (this_half, source=this)
    end select
  end subroutine
end module 

program main
  use lorenz_module
  use runge_kutta_2nd_module ,only : runge_kutta_2nd, integrate
  implicit none

  type(runge_kutta_2nd) :: timed_lorenz_integrator
  type(lorenz)          :: attractor

  attractor = constructor( [1., 1., 1.] , timed_lorenz_integrator)
  call integrate(attractor)
end program main
