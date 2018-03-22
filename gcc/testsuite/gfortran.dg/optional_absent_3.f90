! { dg-do run }
!
! PR fortran/35203
!
! Test VALUE + OPTIONAL
! for integer/real/complex/logical which are passed by value
!
program main
  implicit none
  call value_test ()
contains
  subroutine value_test (ii, rr, cc, ll, ii2, rr2, cc2, ll2)
    integer, optional :: ii, ii2
    real,    optional :: rr, rr2
    complex, optional :: cc, cc2
    logical, optional :: ll, ll2
    value :: ii, rr, cc, ll

    call int_test (.false., 0)
    call int_test (.false., 0, ii)
    call int_test (.false., 0, ii2)
    call int_test (.true., 0, 0)
    call int_test (.true., 2, 2)

    call real_test (.false., 0.0)
    call real_test (.false., 0.0, rr)
    call real_test (.false., 0.0, rr2)
    call real_test (.true., 0.0, 0.0)
    call real_test (.true., 2.0, 2.0)

    call cmplx_test (.false., cmplx (0.0))
    call cmplx_test (.false., cmplx (0.0), cc)
    call cmplx_test (.false., cmplx (0.0), cc2)
    call cmplx_test (.true., cmplx (0.0), cmplx (0.0))
    call cmplx_test (.true., cmplx (2.0), cmplx (2.0))

    call bool_test (.false., .false.)
    call bool_test (.false., .false., ll)
    call bool_test (.false., .false., ll2)
    call bool_test (.true., .false., .false.)
    call bool_test (.true., .true., .true.)
  end subroutine value_test

  subroutine int_test (ll, val, x)
    logical, value :: ll
    integer, value :: val
    integer, value, optional :: x
    if (ll .neqv. present(x)) STOP 1
    if (present(x)) then
      if (x /= val) STOP 1
    endif
  end subroutine int_test

  subroutine real_test (ll, val, x)
    logical, value :: ll
    real, value :: val
    real, value, optional :: x
    if (ll .neqv. present(x)) STOP 2
    if (present(x)) then
      if (x /= val) STOP 2
    endif
  end subroutine real_test

  subroutine cmplx_test (ll, val, x)
    logical, value :: ll
    complex, value :: val
    complex, value, optional :: x
    if (ll .neqv. present(x)) STOP 3
    if (present(x)) then
      if (x /= val) STOP 3
    endif
  end subroutine cmplx_test

  subroutine bool_test (ll, val, x)
    logical, value :: ll
    logical, value :: val
    logical, value, optional :: x
    if (ll .neqv. present(x)) STOP 4
    if (present(x)) then
      if (x .neqv. val) STOP 4
    endif
  end subroutine bool_test
end program main
