!==================assumed_size_refs_1.f90==================
! { dg-do compile }
! Test the fix for PR20868 & PR20870 in which references to
! assumed size arrays without an upper bound to the last
! dimension were generating no error.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
program assumed_size_test_2
  implicit none
  real a(2, 4)

  a = 1.0
  call foo (a)

contains
  subroutine foo(m)
    real, target :: m(1:2, *)
    real x(2,2,2)
    real, pointer :: q(:,:)
    integer :: i
    allocate (q(2,2))

    q = cos (1.0 + abs(m))     ! { dg-error "upper bound in the last dimension" }

    x = reshape (m, (/2,2,2/)) ! { dg-error "upper bound in the last dimension" }

! PR20868
    print *, ubound (m)        ! { dg-error "upper bound in the last dimension" }
    print *, lbound (m)

! PR20870
    print *, size (m)          ! { dg-error "upper bound in the last dimension" }

! Check non-array valued intrinsics
    print *, ubound (m, 1)
    print *, ubound (m, 2)     ! { dg-error "not a valid dimension index" }
    
    i = 2
    print *, size (m, i)

  end subroutine foo

end program assumed_size_test_2

! { dg-error "Cannot simplify expression" " " { target *-*-* } 29 }
