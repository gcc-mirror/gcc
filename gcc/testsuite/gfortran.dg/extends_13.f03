! { dg-do compile }
!
! PR 47601: [OOP] Internal Error: mio_component_ref(): Component not found
!
! Contributed by Rich Townsend <townsend@astro.wisc.edu>

module type_definitions
  implicit none
  type :: matching 
     integer :: n = -999
  end type
  type, extends(matching) :: ellipse
  end type
end module type_definitions

module elliptical_elements
  implicit none
contains
  function line(e) result(a2n)
    use type_definitions
    type(ellipse), intent(in) :: e
    complex, dimension(e%N) :: a2n   ! <- change "e%N" to "10" 
  end function line
end module

  use type_definitions
  use elliptical_elements
end

! { dg-final { cleanup-modules "type_definitions elliptical_elements" } }
