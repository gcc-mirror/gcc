! { dg-do compile }
!
! PR 55297: [4.8 Regression] [OOP] type-bound operator clashes with abstract interface
!
! Contributed by Damian Rouson <damian@rouson.net>

module athlete_module
  type athlete
  contains
    procedure :: negative
    generic :: operator(-) => negative
  end type
  abstract interface 
    integer function sum_interface(this)
      import athlete
      class(athlete) this
    end function
  end interface
contains
  integer function negative(this)
    class(athlete) ,intent(in) :: this
  end function
end module 

! { dg-final { cleanup-modules "athlete_module" } }
