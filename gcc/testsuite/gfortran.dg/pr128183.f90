! { dg-do run }
! { dg-additional-sources pr128183_aux.c }
!
! PR libfortran/128183
! Formatted I/O must leave a non-"C" LC_NUMERIC unchanged.  On targets without
! uselocale (e.g. MinGW-w64), libgfortran used to restore LC_NUMERIC through a
! pointer invalidated by its own setlocale (LC_NUMERIC, "C") call.  Targets
! with uselocale never call setlocale during I/O, so on those targets this test
! cannot fail.  The ' 1.5' check also catches a missing switch to "C" when the
! chosen locale uses a decimal comma.

program pr128183
  use iso_c_binding, only: c_int
  implicit none
  interface
    integer(c_int) function set_non_c_locale () bind(C)
      import :: c_int
    end function set_non_c_locale
    integer(c_int) function locale_unchanged () bind(C)
      import :: c_int
    end function locale_unchanged
  end interface
  character(len=8) :: buf
  integer :: i

  ! Nothing to test if no locale other than "C" is installed.
  if (set_non_c_locale () == 0) stop

  do i = 1, 3
    write (buf, '(F4.1)') 1.5
    if (buf /= ' 1.5') stop 1
    if (locale_unchanged () == 0) stop 2
  end do
end program pr128183
