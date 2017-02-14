! { dg-do compile }
!
! PR 60853: [OOP] Failure to disambiguate generic with unlimited polymorphic
!
! Contributed by tlcclt <Thomas.L.Clune@nasa.gov>

module foo_mod
   implicit none

   type Vector
   contains
      procedure :: copyFromScalar
      procedure :: copyFromArray
      generic :: assignment(=) => copyFromScalar, copyFromArray
   end type

contains

   subroutine copyFromScalar(this, scalar)
      class (Vector), intent(inout) :: this
      type  (Vector), intent(in) :: scalar
   end subroutine

   subroutine copyFromArray(this, array)
      class (Vector), intent(inout) :: this
      class (*), intent(in) :: array(:)
   end subroutine

end module
