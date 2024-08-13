! { dg-do compile }
!
! PR fortran/114308 - reject array constructor value of abstract type

module my_module
  implicit none
  private

  type, abstract, public :: a
  end type

  type, extends(a), public :: b
  end type
end

program main
  use my_module
  implicit none
  type(b)               :: b_instance
  class(a), allocatable :: a_array(:)
  class(b), allocatable :: b_array(:)

  a_array = [b_instance]
  b_array = [b_instance]
  a_array = [a_array]             ! { dg-error "is of the ABSTRACT type" }
  a_array = [a_array(1)]          ! { dg-error "is of the ABSTRACT type" }
  a_array = [a_array, b_instance] ! { dg-error "is of the ABSTRACT type" }
  a_array = [b_instance, a_array] ! { dg-error "is of the ABSTRACT type" }
  b_array = [b_array, a_array]    ! { dg-error "is of the ABSTRACT type" }
end program
