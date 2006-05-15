! { dg-do run }
! Tests the fix for PR27411, in which the array reference on line
! 18 caused an ICE because the derived type, rather than its integer
! component, was appearing in the index expression.
!
! Contributed by Richard Maine  <1fhcwee02@sneakemail.com>
!
module gd_calc
  type calc_signal_type
    integer :: dummy
    logical :: used
    integer :: signal_number
  end type
contains
  subroutine activate_gd_calcs (used, outputs)
    logical, intent(inout) :: used(:)
    type(calc_signal_type), pointer :: outputs(:)
      outputs%used = used(outputs%signal_number)
    return
  end subroutine activate_gd_calcs
end module gd_calc

  use gd_calc
  integer, parameter :: ndim = 4
  integer :: i
  logical :: used_(ndim)
  type(calc_signal_type), pointer :: outputs_(:)
  allocate (outputs_(ndim))
  forall (i = 1:ndim) outputs_(i)%signal_number = ndim + 1 - i
  used_ = (/.true., .false., .true., .true./)
  call activate_gd_calcs (used_, outputs_)
  if (any (outputs_(ndim:1:-1)%used .neqv. used_)) call abort ()
end

! { dg-final { cleanup-modules "gd_calc" } }
