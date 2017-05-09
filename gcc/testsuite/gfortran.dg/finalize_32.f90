! { dg-do compile }
!
! PR 79311: [OOP] ICE in generate_finalization_wrapper, at fortran/class.c:1992
!
! Contributed by DIL <liakhdi@ornl.gov>

module tensor_recursive
  implicit none

  type :: tens_signature_t
  contains
    final :: tens_signature_dtor
  end type

  type :: tens_header_t
    type(tens_signature_t) :: signature
  contains
    final :: tens_header_dtor
  end type

contains

  subroutine tens_signature_dtor(this)
    type(tens_signature_t) :: this
  end subroutine

  subroutine tens_header_dtor(this)
    type(tens_header_t) :: this
  end subroutine

end
