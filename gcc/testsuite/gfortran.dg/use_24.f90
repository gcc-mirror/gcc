! { dg-do run }
!
! PR fortran/42769
! The static resolution of A%GET used to be incorrectly simplified to MOD2's
! MY_GET instead of the original MOD1's MY_GET, depending on the order in which
! MOD1 and MOD2 were use-associated.
!
! Original testcase by Salvator Filippone <sfilippone@uniroma2.it>
! Reduced by Janus Weil <janus@gcc.gnu.org>

module mod1
  type :: t1
  contains
    procedure, nopass :: get => my_get
  end type
contains 
  subroutine my_get(i)
    i = 2
  end subroutine
end module

module mod2
contains 
  subroutine my_get(i)    ! must have the same name as the function in mod1
    i = 5
  end subroutine
end module


 call test1()
 call test2()

contains

 subroutine test1()
  use mod2
  use mod1
  type(t1) :: a
  call a%get(j)
  if (j /= 2) STOP 1
 end subroutine test1

 subroutine test2()
  use mod1
  use mod2
  type(t1) :: a
  call a%get(j)
  if (j /= 2) STOP 2
 end subroutine test2
end



