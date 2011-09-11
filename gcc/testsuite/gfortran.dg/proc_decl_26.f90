! { dg-do compile }
!
! PR 35831: [F95] Shape mismatch check missing for dummy procedure argument
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

program test

  implicit none

  interface
    subroutine one(a)
      integer a(:)
    end subroutine
    subroutine two(a)
      integer a(2)
    end subroutine
  end interface

  call foo(two)  ! { dg-error "Shape mismatch in argument" }
  call bar(two)  ! { dg-error "Shape mismatch in argument" }

contains

  subroutine foo(f1)
    procedure(one) :: f1
  end subroutine foo

  subroutine bar(f2)
    interface
      subroutine f2(a)
        integer a(:)
      end subroutine
    end interface
  end subroutine bar

end program 
