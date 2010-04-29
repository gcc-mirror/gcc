! { dg-do compile }
!
! PR 42274: [fortran-dev Regression] ICE: segmentation fault
!
! Original test case by Salvatore Filippone <sfilippone@uniroma2.it>
! Modified by Janus Weil <janus@gcc.gnu.org>

module mod_A
  type :: t1
  contains 
    procedure,nopass :: fun
  end type
contains
  logical function fun()
  end function
end module

module mod_B
  use mod_A
  type, extends(t1) :: t2
  contains
    procedure :: sub1
  end type
contains 
  subroutine sub1(a)
    class(t2) :: a
  end subroutine
end module

module mod_C
contains 
  subroutine sub2(b)
    use mod_B
    type(t2) :: b
  end subroutine
end module

module mod_D
  use mod_A
  use mod_C
end module

! { dg-final { cleanup-modules "mod_A mod_B mod_C mod_D" } }
