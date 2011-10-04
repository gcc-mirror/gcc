! { dg-do compile }
!
! PR 35381: [F95] Shape mismatch check missing for dummy procedure argument
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module m

  implicit none

contains

  ! constant array bounds

  subroutine s1(a)
    integer :: a(1:2)
  end subroutine

  subroutine s2(a)
    integer :: a(2:3)
  end subroutine

  subroutine s3(a)
    integer :: a(2:4)
  end subroutine

  ! non-constant array bounds

  subroutine t1(a,b)
    integer :: b
    integer :: a(1:b,1:b)
  end subroutine

  subroutine t2(a,b)
    integer :: b
    integer :: a(1:b,2:b+1)
  end subroutine

  subroutine t3(a,b)
    integer :: b
    integer :: a(1:b,1:b+1)
  end subroutine

end module


program test
  use m
  implicit none

  call foo(s1)  ! legal
  call foo(s2)  ! legal
  call foo(s3)  ! { dg-error "Shape mismatch in dimension" }

  call bar(t1)  ! legal
  call bar(t2)  ! legal
  call bar(t3)  ! { dg-error "Shape mismatch in dimension" }

contains

  subroutine foo(f)
    procedure(s1) :: f
  end subroutine

  subroutine bar(f)
    procedure(t1) :: f
  end subroutine

end program

! { dg-final { cleanup-modules "m" } }
