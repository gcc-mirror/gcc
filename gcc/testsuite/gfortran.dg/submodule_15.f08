! { dg-do run }
!
! Check the fix for PR69524, where module procedures were not permitted
! in a module CONTAINS section.
!
! Reorted by Kirill Yukhin  <kyukhin@gcc.gnu.org>
!
module A
  implicit none
  interface
     module subroutine A1(i)
       integer, intent(inout) :: i
     end subroutine A1
     module subroutine A2(i)
       integer, intent(inout) :: i
     end subroutine A2
     integer module function A3(i)
       integer, intent(inout) :: i
     end function A3
     module subroutine B1(i)
       integer, intent(inout) :: i
     end subroutine B1
  end interface
  integer :: incr         ! Make sure that everybody can access a module variable
contains
  module subroutine A1(i) ! Full declaration
    integer, intent(inout) :: i
    call b1 (i)           ! Call the submodule procedure
    incr = incr + 1
  end subroutine A1

  module PROCEDURE A2     ! Abreviated declaration
    call b1 (i)           ! Call the submodule procedure
    incr = incr + 1
  end procedure A2

  module PROCEDURE A3     ! Abreviated declaration
    call a1 (i)           ! Call the module procedure in the module
    call a2 (i)           !            ditto
    call b1 (i)           ! Call the submodule procedure
    incr = incr + 1
    a3 = i + incr
  end procedure A3
end module A

submodule (A) a_son
  implicit none
contains
  module procedure b1
    i = i + incr
  end procedure
end submodule

  use A
  integer :: i = 1
  incr = 1
  if (a3(i) .ne. 11) call abort
end
! { dg-final { cleanup-submodules "a@a_son" } }
