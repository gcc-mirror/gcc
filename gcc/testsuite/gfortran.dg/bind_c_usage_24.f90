! { dg-do run }
! { dg-additional-sources bind_c_usage_24_c.c }
!
! PR fortran/48858
! PR fortran/48820
!
! TS 29113: BIND(C) with OPTIONAL
!
module m
  use iso_c_binding
  interface
    subroutine c_proc (is_present, var) bind(C)
      import
      logical(c_bool), value    :: is_present
      integer(c_int),  optional :: var
    end subroutine
  end interface
contains
  subroutine subtest (is_present, var) bind(C)
    logical(c_bool), intent(in),    value    :: is_present
    integer(c_int),  intent(inout), optional :: var
    if (is_present) then
      if (.not. present (var)) STOP 1
      if (var /= 43) STOP 2
      var = -45
    else
      if (present (var)) STOP 3
    end if
  end subroutine subtest
end module m

program test
  use m
  implicit none
  integer :: val

  val = 4
  call c_proc (.false._c_bool)
  call c_proc (.true._c_bool, val)
  if (val /= 7) STOP 4
end program test
