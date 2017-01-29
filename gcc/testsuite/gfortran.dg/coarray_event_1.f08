! { dg-do compile }
! { dg-options "-fcoarray=lib -lcaf_single" }

! Check that pr70696 is really fixed.

  use iso_fortran_env
  type(event_type) :: x[*]

  ! exchange must not be called or the link problem before the patch
  ! does not occur.
contains
  subroutine exchange
    event post (x[1])
  end subroutine
end 
