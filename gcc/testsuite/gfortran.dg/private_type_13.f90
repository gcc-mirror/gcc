! { dg-do compile }
! Test fix for F95 part of PR39800, in which the host association of the type 't1'
! generated an error.
!
! Reported to clf by Alexei Matveev <Alexei Matveev@gmail.com> and reported by
! Tobias Burnus  <burnus@gcc.gnu.org>
!
module m
  implicit none
  private

  type :: t1
    integer :: i
  end type

  type :: t2
    type(t1) :: j
  end type

  contains

    subroutine sub()
      implicit none

      type :: t3
        type(t1) :: j
      end type

    end subroutine

end module
