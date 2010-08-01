! { dg-do run }
!
! PR 44912: [OOP] Segmentation fault on TBP
!
! Contributed by Satish.BD <bdsatish@gmail.com>

module polynomial
implicit none

private

type, public :: polynom
   complex, allocatable, dimension(:) :: a
   integer :: n
 contains
   procedure :: init_from_coeff
   procedure :: get_degree
   procedure :: add_poly
end type polynom

contains
  subroutine init_from_coeff(self, coeff)
    class(polynom), intent(inout) :: self
    complex, dimension(:), intent(in) :: coeff
    self%n = size(coeff) - 1
    allocate(self%a(self%n + 1))
    self%a = coeff
    print *,"ifc:",self%a
  end subroutine init_from_coeff

  function get_degree(self)   result(n)
    class(polynom), intent(in) :: self
    integer :: n
    print *,"gd"
    n = self%n
  end function get_degree

  subroutine add_poly(self)
    class(polynom), intent(in) :: self
    integer :: s
    print *,"ap"
    s = self%get_degree()         !!!! fails here
  end subroutine

end module polynomial

program test_poly
   use polynomial, only: polynom

   type(polynom) :: p1

   call p1%init_from_coeff([(1,0),(2,0),(3,0)])
   call p1%add_poly()

end program test_poly

! { dg-final { cleanup-modules "polynomial" } }
