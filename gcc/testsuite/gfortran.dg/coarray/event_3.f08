! { dg-do run }
!
! Check PR fortran/70696 is fixed.

program global_event
  use iso_fortran_env, only : event_type
  implicit none
  type(event_type), save :: x[*]
  
  call exchange
  contains
    subroutine exchange
      integer :: cnt
      event post(x[1])
      event post(x[1])
      call event_query(x, cnt)
      if (cnt /= 2) error stop 1
      event wait(x, until_count=2)
    end subroutine
end 
