! { dg-do compile }
!
! PR 48706: Type extension inside subroutine
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

module mod_diff_01
  implicit none
  type :: foo
  end type
contains
  subroutine create_ext
    type, extends(foo) :: foo_e
    end type
  end subroutine
end module

program diff_01
  use mod_diff_01
  implicit none
  call create_ext()
end program

! { dg-final { cleanup-modules "mod_diff_01" } }
