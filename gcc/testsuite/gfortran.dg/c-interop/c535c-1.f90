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

module t
  type :: t1
    integer :: id
    real :: xyz(3)
  end type
end module  

module m
  use t

  ! Assumed-type dummies are (unlimited) polymorphic too, but F2018:C709
  ! already prohibits them from being declared intent(out).  So we only
  ! test dummies of class type that are polymorphic or unlimited
  ! polymorphic.
  interface
    subroutine poly (x, y)
      use t
      class(t1) :: x(..)
      class(t1), intent (out) :: y(..)
    end subroutine
    subroutine upoly (x, y)
      class(*) :: x(..)
      class(*), intent (out) :: y(..)
    end subroutine
  end interface

contains

  ! The known-size calls should all be OK as they do not involve
  ! assumed-size or assumed-rank actual arguments.
  subroutine test_known_size_nonpolymorphic (a1, a2, n)
    integer :: n
    type(t1) :: a1(n,n), a2(n)
    call poly (a1, a2)
    call upoly (a1, a2)
  end subroutine
  subroutine test_known_size_polymorphic (a1, a2, n)
    integer :: n
    class(t1) :: a1(n,n), a2(n)
    call poly (a1, a2)
    call upoly (a1, a2)
  end subroutine
  subroutine test_known_size_unlimited_polymorphic (a1, a2, n)
    integer :: n
    class(*) :: a1(n,n), a2(n)
    call upoly (a1, a2)
  end subroutine

  ! Likewise passing a scalar as the assumed-rank argument.
  subroutine test_scalar_nonpolymorphic (a1, a2)
    type(t1) :: a1, a2
    call poly (a1, a2)
    call upoly (a1, a2)
  end subroutine
  subroutine test_scalar_polymorphic (a1, a2)
    class(t1) :: a1, a2
    call poly (a1, a2)
    call upoly (a1, a2)
  end subroutine
  subroutine test_scalar_unlimited_polymorphic (a1, a2)
    class(*) :: a1, a2
    call upoly (a1, a2)
  end subroutine
  
  ! The polymorphic cases for assumed-size are bad.
  subroutine test_assumed_size_nonpolymorphic (a1, a2)
    type(t1) :: a1(*), a2(*)
    call poly (a1, a2)  ! OK
    call upoly (a1, a2)  ! OK
  end subroutine
  subroutine test_assumed_size_polymorphic (a1, a2)
    class(t1) :: a1(*), a2(*)
    call poly (a1, a2)  ! { dg-error "(A|a)ssumed.rank" }
    call upoly (a1, a2)  ! { dg-error "(A|a)ssumed.rank" }
    call poly (a1(5), a2(4:7))
  end subroutine
  subroutine test_assumed_size_unlimited_polymorphic (a1, a2)
    class(*) :: a1(*), a2(*)
    call upoly (a1, a2)  ! { dg-error "(A|a)ssumed.rank" }
  end subroutine

  ! The arguments being passed to poly/upoly in this set are *not*
  ! assumed size and should not error.
  subroutine test_not_assumed_size_nonpolymorphic (a1, a2)
    type(t1) :: a1(*), a2(*)
    call poly (a1(5), a2(4:7))
    call upoly (a1(5), a2(4:7))
    call poly (a1(:10), a2(:-5))
    call upoly (a1(:10), a2(:-5))
  end subroutine
  subroutine test_not_assumed_size_polymorphic (a1, a2)
    class(t1) :: a1(*), a2(*)
    call poly (a1(5), a2(4:7))
    call upoly (a1(5), a2(4:7))
    call poly (a1(:10), a2(:-5))
    call upoly (a1(:10), a2(:-5))
  end subroutine
  subroutine test_not_assumed_size_unlimited_polymorphic (a1, a2)
    class(*) :: a1(*), a2(*)
    call upoly (a1(5), a2(4:7))
    call upoly (a1(:10), a2(:-5))
  end subroutine

  ! Polymorphic assumed-rank without pointer/allocatable is also bad.
  subroutine test_assumed_rank_nonpolymorphic (a1, a2)
    type(t1) :: a1(..), a2(..)
    call poly (a1, a2)  ! OK
    call upoly (a1, a2)  ! OK
  end subroutine
  subroutine test_assumed_rank_polymorphic (a1, a2)
    class(t1) :: a1(..), a2(..)
    call poly (a1, a2)  ! { dg-error "(A|a)ssumed.rank" }
    call upoly (a1, a2)  ! { dg-error "(A|a)ssumed.rank" }
  end subroutine
  subroutine test_assumed_rank_unlimited_polymorphic (a1, a2)
    class(*) :: a1(..), a2(..)
    call upoly (a1, a2)  ! { dg-error "(A|a)ssumed.rank" }
  end subroutine

  ! Pointer/allocatable assumed-rank should be OK.
  subroutine test_pointer_nonpolymorphic (a1, a2)
    type(t1), pointer :: a1(..), a2(..)
    call poly (a1, a2)
    call upoly (a1, a2)
  end subroutine
  subroutine test_pointer_polymorphic (a1, a2)
    class(t1), pointer :: a1(..), a2(..)
    call poly (a1, a2)
    call upoly (a1, a2)
  end subroutine
  subroutine test_pointer_unlimited_polymorphic (a1, a2)
    class(*), pointer :: a1(..), a2(..)
    call upoly (a1, a2)
  end subroutine

  subroutine test_allocatable_nonpolymorphic (a1, a2)
    type(t1), allocatable :: a1(..), a2(..)
    call poly (a1, a2)
    call upoly (a1, a2)
  end subroutine
  subroutine test_allocatable_polymorphic (a1, a2)
    class(t1), allocatable :: a1(..), a2(..)
    call poly (a1, a2)
    call upoly (a1, a2)
  end subroutine
  subroutine test_allocatable_unlimited_polymorphic (a1, a2)
    class(*), allocatable :: a1(..), a2(..)
    call upoly (a1, a2)
  end subroutine

end module
