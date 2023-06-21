! { dg-do compile }
!
! TS 29113
! The definition of TKR compatible in paragraph 2 of subclause 12.4.3.4.5
! of ISO/IEC 1539-1:2010 is changed to:
!
! A dummy argument is type, kind, and rank compatible, or TKR compatible,
! with another dummy argument if the first is type compatible with the
! second, the kind type parameters of the first have the same values as
! the corresponding kind type parameters of the second, and both have the
! same rank or either is assumed-rank.
!
! This test file contains tests that are expected to issue diagnostics
! for invalid code.

module m

interface foo
  subroutine foo_1 (x)  ! { dg-error "Ambiguous interfaces" }
    integer :: x(..)
  end subroutine
  subroutine foo_2 (x)  ! { dg-error "Ambiguous interfaces" }
    integer :: x(:, :)
  end subroutine
end interface

interface bar
  subroutine bar_1 (x)  ! { dg-error "Ambiguous interfaces" }
    integer :: x(..)
  end subroutine
  subroutine bar_2 (x)  ! { dg-error "Ambiguous interfaces" }
    integer :: x(..)
  end subroutine
end interface

interface baz
  subroutine baz_1 (x)  ! { dg-error "Ambiguous interfaces" }
    integer :: x
  end subroutine
  subroutine baz_2 (x)  ! { dg-error "Ambiguous interfaces" }
    integer :: x(..)
  end subroutine
end interface

end module

