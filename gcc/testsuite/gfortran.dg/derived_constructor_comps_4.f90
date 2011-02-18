! { dg-do run }
!
! PR 47789: [F03] Structure constructor of type extending DT with no components
!
! Contributed by eddyg_61-bugzilla@yahoo.it

type:: one
end type

type, extends(one) :: two
  integer :: a
end type

type(two) :: wo = two(6)

if (wo%a /= 6) call abort()

end
