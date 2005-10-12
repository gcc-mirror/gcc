! { dg-do compile }
! { dg-options "-O0" }
! Tests the fix for PR24207 and the problems associated
! with the fix for PR21986. In two cases, use associated
! public symbols were taking on the default private access
! attribute of the local namespace. In the third, a private
! symbol was not available to a namelist in contained 
! procedure in the same module.
!
! Based on the example in PR24207.
!
module a
  implicit none
  real b
  type :: mytype
    integer :: c
  end type mytype
end module a
module c
  use a
  implicit none
  public d
  private
  real x
  contains
     subroutine d (arg_t)   ! This would cause an error
        type (mytype) :: arg_t
        namelist /e/ b, x   ! .... as would this.
        read(5,e)
	arg_t%c = 42
     end subroutine d
end module c

