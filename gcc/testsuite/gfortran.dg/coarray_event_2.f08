! { dg-do compile }
! { dg-options "-fcoarray=lib -lcaf_single" }

! Check that pr79866 is really fixed.

  use iso_fortran_env
  type(event_type) :: x ! { dg-error "of type EVENT_TYPE or with subcomponent of type EVENT_TYPE must be a coarray" }

contains
  subroutine exchange
    event post (x[1]) ! { dg-error "Syntax error in EVENT POST statement at .1." }
  end subroutine
end 
