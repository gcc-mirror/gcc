! { dg-do compile }
! Test the fix for PR20903, in which derived types could be host associated within
! interface bodies.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
! 
module test
  implicit none
  type fcnparms
    integer :: i
  end type fcnparms
contains
  subroutine sim_1(func1,params)
    interface
      function func1(fparams)
        type(fcnparms) :: fparams ! { dg-error "not been declared within the interface" }
        real :: func1
      end function func1
    end interface
    type(fcnparms)     :: params
   end subroutine sim_1

  subroutine sim_2(func2,params)
    interface
      function func2(fparams)     ! This is OK because of the derived type decl.
        type fcnparms
          integer :: i
        end type fcnparms
        type(fcnparms)  :: fparams
        real :: func2
      end function func2
    end interface
    type(fcnparms)      :: params ! This is OK, of course
   end subroutine sim_2
end module  test

module type_decl
  implicit none
  type fcnparms
    integer :: i
  end type fcnparms
end module type_decl

subroutine sim_3(func3,params)
  use type_decl
  interface
    function func3(fparams)
      use type_decl
      type(fcnparms)   :: fparams ! This is OK - use associated
      real :: func3
    end function func3
  end interface
  type(fcnparms)       :: params  !         -ditto-
end subroutine sim_3
