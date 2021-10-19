! PR 54753
! { dg-do compile }
!
! TS 29113
! C535c If an assumed-size or nonallocatable nonpointer assumed-rank
! array is an actual argument corresponding to a dummy argument that 
! is an INTENT(OUT) assumed-rank array, it shall not be [...] 
! of a type for which default initialization is specified.
!
! This constraint is numbered C839 in the Fortran 2018 standard.
!
! This test file contains tests that are expected to issue diagnostics
! for invalid code.

module m

  type :: t1
    integer :: id
    real :: xyz(3)
    integer :: tag = -1
  end type

contains

  subroutine finalize_t1 (obj)
    type(t1) :: obj
  end subroutine

  subroutine s1 (x, y)
    type(t1) :: x(..)
    type(t1), intent(out) :: y(..)
  end subroutine

  ! This call should be OK as it does not involve assumed-size or
  ! assumed-rank actual arguments.
  subroutine test_known_size (a1, a2, n)
    integer :: n
    type(t1) :: a1(n,n), a2(n)

    call s1 (a1, a2)
  end subroutine

  ! Calls with an assumed-size array argument should be rejected.
  subroutine test_assumed_size (a1, a2)
    type(t1) :: a1(*), a2(*)
    
    call s1 (a1, a2)  ! { dg-error "(A|a)ssumed.rank" }
  end subroutine

  ! This call should be OK.
  subroutine test_assumed_rank_pointer (a1, a2)
    type(t1), pointer :: a1(..), a2(..)

    call s1 (a1, a2)
  end subroutine

  ! This call should be OK.
  subroutine test_assumed_rank_allocatable (a1, a2)
    type(t1), allocatable :: a1(..), a2(..)

    call s1 (a1, a2)
  end subroutine
  
  ! The call should be rejected with a nonallocatable nonpointer
  ! assumed-rank actual argument.
  subroutine test_assumed_rank_plain (a1, a2)
    type(t1) :: a1(..), a2(..)

    call s1 (a1, a2)  ! { dg-error "(A|a)ssumed.rank" }
  end subroutine

end module
