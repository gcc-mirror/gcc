! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/48112 (module_m)
! PR fortran/48279 (sidl_string_array, s_Hard)
!
! Contributed by mhp77@gmx.at (module_m)
! and Adrian Prantl (sidl_string_array, s_Hard)
!

module module_m
  interface test
     function test1( )  result( test )
       integer ::  test
     end function test1
  end interface test
end module module_m

! -----

module sidl_string_array
  type sidl_string_1d
  end type sidl_string_1d
  interface set
    module procedure &
      setg1_p
  end interface
contains
  subroutine setg1_p(array, index, val)
    type(sidl_string_1d), intent(inout) :: array
  end subroutine setg1_p
end module sidl_string_array

module s_Hard
  use sidl_string_array
  type :: s_Hard_t
     integer(8) :: dummy
  end type s_Hard_t
  interface set_d_interface
  end interface 
  interface get_d_string
    module procedure get_d_string_p
  end interface 
  contains ! Derived type member access functions
    type(sidl_string_1d) function get_d_string_p(s)
      type(s_Hard_t), intent(in) :: s
    end function get_d_string_p
    subroutine set_d_objectArray_p(s, d_objectArray)
    end subroutine set_d_objectArray_p
end module s_Hard

subroutine initHard(h, ex)
  use s_Hard
  type(s_Hard_t), intent(inout) :: h
  call set(get_d_string(h), 0, 'Three') ! { dg-error "There is no specific subroutine for the generic" }
end subroutine initHard

! -----

  interface get
    procedure get1
  end interface

  integer :: h
  call set1 (get (h))

contains

  subroutine set1 (a)
    integer, intent(in) :: a
  end subroutine

  integer function get1 (s) ! { dg-error "Fortran 2008: Internal procedure .get1. in generic interface .get." }
    integer :: s
  end function

end
