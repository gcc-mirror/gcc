! { dg-do run }
! { dg-additional-sources c_ptr_tests_driver.c }
module c_ptr_tests
  use, intrinsic :: iso_c_binding

  ! TODO::
  ! in order to be associated with a C address, 
  ! the derived type needs to be C interoperable, 
  ! which requires bind(c) and all fields interoperable.
  type, bind(c) :: myType
     type(c_ptr) :: myServices
     type(c_funptr) :: mySetServices
     type(c_ptr) :: myPort
  end type myType

  type, bind(c) :: f90Services
     integer(c_int) :: compId
     type(c_ptr) :: globalServices = c_null_ptr
  end type f90Services

  contains
    
    subroutine sub0(c_self, services) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), value :: c_self, services
      type(myType), pointer :: self
      type(f90Services), pointer :: localServices
!      type(c_ptr) :: my_cptr 
      type(c_ptr), save :: my_cptr = c_null_ptr

      call c_f_pointer(c_self, self)
      if(.not. associated(self)) then
         print *, 'self is not associated'
      end if
      self%myServices = services

      ! c_null_ptr is defined in iso_c_binding
      my_cptr = c_null_ptr

      ! get access to the local services obj from C
      call c_f_pointer(self%myServices, localServices)
    end subroutine sub0
end module c_ptr_tests

! { dg-final { cleanup-modules "c_ptr_tests" } }
