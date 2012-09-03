! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/51632
!
! Was rejected before as __def_init and __copy were
! resolved and coarray components aren't valid in this
! context
!
module periodic_2nd_order_module
  implicit none

  type periodic_2nd_order
    real, allocatable :: global_f(:)[:]
  contains
    procedure :: output
  end type

contains
  subroutine output (this)
    class(periodic_2nd_order), intent(in) :: this
  end subroutine
end module
