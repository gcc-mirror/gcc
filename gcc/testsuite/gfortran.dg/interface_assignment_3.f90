! { dg-do compile }
! Checks the fix for PR34008, in which INTENT(INOUT) was disallowed
! for the first argument of assign_m, whereas both INOUT and OUT
! should be allowed.
!
! Contributed by Harald Anlauf <anlauf@gmx.de> 
!
module mo_memory
  implicit none
  type t_mi
     logical       :: alloc = .false.
  end type t_mi
  type t_m
     type(t_mi)    :: i                         ! meta data
     real, pointer :: ptr (:,:,:,:) => NULL ()
  end type t_m

  interface assignment (=)
     module  procedure assign_m
  end interface
contains
  elemental subroutine assign_m (y, x)
    !---------------------------------------
    ! overwrite intrinsic assignment routine
    !---------------------------------------
    type (t_m), intent(inout) :: y
    type (t_m), intent(in)    :: x
    y% i = x% i
    if (y% i% alloc) y% ptr = x% ptr
  end subroutine assign_m
end module mo_memory

module gfcbug74
  use mo_memory, only: t_m, assignment (=)
  implicit none
  type t_atm
     type(t_m) :: m(42)
  end type t_atm
contains
  subroutine assign_atm_to_atm (y, x)
    type (t_atm), intent(inout) :: y
    type (t_atm), intent(in)    :: x
    integer :: i
!   do i=1,42; y% m(i) = x% m(i); end do    ! Works
    y% m = x% m                             ! ICE
  end subroutine assign_atm_to_atm
end module gfcbug74
! { dg-final { cleanup-modules "mo_memory gfcbug74" } }

