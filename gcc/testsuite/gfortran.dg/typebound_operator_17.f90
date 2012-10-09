! { dg-do compile }
!
! PR 54832: [4.8 Regression] [OOP] Type-bound operator not picked up with RESULT variable
!
! Contributed by Damian Rouson <rouson@sandia.gov>

  type, abstract :: integrand
  contains
    procedure(t_interface), deferred :: t
    procedure(assign_interface), deferred :: assign
    procedure(times_interface), deferred :: times
    generic :: operator(*) => times
    generic :: assignment(=) => assign
  end type

  abstract interface
    function t_interface(this) result(dState_dt)
      import :: integrand
      class(integrand) ,intent(in)  :: this
      class(integrand) ,allocatable :: dState_dt
    end function
    function times_interface(lhs,rhs)
      import :: integrand
      class(integrand) ,intent(in)  :: lhs
      class(integrand) ,allocatable :: times_interface
      real, intent(in)  :: rhs
    end function
    subroutine assign_interface(lhs,rhs)
      import :: integrand
      class(integrand) ,intent(in)    :: rhs
      class(integrand) ,intent(inout) :: lhs
    end subroutine
  end interface

contains

  subroutine integrate(model,dt)
    class(integrand) :: model
    real dt
    model = model%t()*dt
   end subroutine

end 
