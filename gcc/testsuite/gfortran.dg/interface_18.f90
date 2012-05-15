! { dg-do compile }
! { dg-options "-std=f2003" }
!
! Public procedures with private types for the dummies
! is valid F2003, but invalid per Fortran 95, Sect. 5.2.3
! See interface_15.f90 for the F95 test case.
!
   module mytype_application
     implicit none
     private
     public :: mytype_test
     type :: mytype_type
       integer :: i=0
     end type mytype_type
   contains
     subroutine mytype_test( mytype )
       type(mytype_type), intent(in out) :: mytype
     end subroutine mytype_test
   end module mytype_application 
