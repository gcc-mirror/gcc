! { dg-do compile }
! PR fortran/90519

module pr90519_finalizer_mod
  implicit none
  type :: t
     type(t), allocatable :: child
  contains
     final :: finalize_t
  end type t
contains
  subroutine finalize_t(self)
    type(t), intent(inout) :: self
  end subroutine finalize_t
end module pr90519_finalizer_mod
