! { dg-do compile }
! PR51791 and original testcase for PR46328.
!
! Contributer by Thomas Koenig  <tkoenig@gcc.gnu.org>
!
module field_module
  implicit none
  type ,abstract :: field
  contains
    procedure(field_op_real) ,deferred :: multiply_real
    generic :: operator(*) => multiply_real
  end type
  abstract interface
    function field_op_real(lhs,rhs)
      import :: field
      class(field) ,intent(in)  :: lhs
      real ,intent(in) :: rhs
      class(field) ,allocatable :: field_op_real
    end function
  end interface
end module

program main
  use field_module
  implicit none
  class(field) ,pointer :: u
  u = (u)*2. ! { dg-error "check that there is a matching specific" }
end program
! { dg-final { cleanup-modules "field_module" } }
