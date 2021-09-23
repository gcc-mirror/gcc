! PR 54753
! { dg-do compile}
!
! TS 29113
! C535c If an assumed-size or nonallocatable nonpointer assumed-rank
! array is an actual argument corresponding to a dummy argument that 
! is an INTENT(OUT) assumed-rank array, it shall not be polymorphic, [...].
!
! This constraint is numbered C839 in the Fortran 2018 standard.
!
! This test file contains tests that are expected to issue diagnostics
! for invalid code.

module m

  type :: t1
    integer :: id
    real :: xyz(3)
  end type

contains

  subroutine s1_nonpolymorphic (x, y)
    type(t1) :: x(..)
    type(t1), intent(out) :: y(..)
  end subroutine

  subroutine s1_polymorphic (x, y)  ! { dg-bogus "(A|a)ssumed.rank" "pr54753" { xfail *-*-* } }
    class(t1) :: x(..)
    class(t1), intent(out) :: y(..)
  end subroutine

  subroutine s1_unlimited_polymorphic (x, y)  ! { dg-bogus "(A|a)ssumed.rank" "pr54753" { xfail *-*-* } }
    class(*) :: x(..)
    class(*), intent(out) :: y(..)
  end subroutine

  ! These calls should all be OK as they do not involve assumed-size or
  ! assumed-rank actual arguments.
  subroutine test_known_size (a1, a2, n)
    integer :: n
    type(t1) :: a1(n,n), a2(n)

    call s1_nonpolymorphic (a1, a2)
    call s1_polymorphic (a1, a2)
    call s1_unlimited_polymorphic (a1, a2)
  end subroutine

  ! The calls to the polymorphic functions should be rejected
  ! with an assumed-size array argument.
  subroutine test_assumed_size (a1, a2)
    type(t1) :: a1(*), a2(*)

    call s1_nonpolymorphic (a1, a2)
    call s1_polymorphic (a1, a2)  ! { dg-error "(A|a)ssumed.rank" "pr54753" { xfail *-*-* } }
    call s1_unlimited_polymorphic (a1, a2) ! { dg-error "(A|a)ssumed.rank" "pr54753" { xfail *-*-* } }
  end subroutine

  ! These calls should be OK.
  subroutine test_assumed_rank_pointer (a1, a2)
    type(t1), pointer :: a1(..), a2(..)

    call s1_nonpolymorphic (a1, a2)
    call s1_polymorphic (a1, a2)
    call s1_unlimited_polymorphic (a1, a2)
  end subroutine

  ! These calls should be OK.
  subroutine test_assumed_rank_allocatable (a1, a2)
    type(t1), allocatable :: a1(..), a2(..)

    call s1_nonpolymorphic (a1, a2)
    call s1_polymorphic (a1, a2)
    call s1_unlimited_polymorphic (a1, a2)
  end subroutine
  
  ! The calls to the polymorphic functions should be rejected
  ! with a nonallocatable nonpointer assumed-rank actual argument.
  subroutine test_assumed_rank_plain (a1, a2)
    type(t1) :: a1(..), a2(..)

    call s1_nonpolymorphic (a1, a2)
    call s1_polymorphic (a1, a2)  ! { dg-error "(A|a)ssumed.rank" "pr54753" { xfail *-*-* } }
    call s1_unlimited_polymorphic (a1, a2)  ! { dg-error "(A|a)ssumed.rank" "pr54753" { xfail *-*-* } }
  end subroutine

end module
