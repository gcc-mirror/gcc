! { dg-do run }
! Tests the fix for PR40591 in which the interface 'sub2'
! for 'pptr2' was not resolved.
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
program main
  call test
contains
  subroutine sub1(arg)
    integer arg
    arg = arg + 1
  end subroutine sub1
  subroutine test()
    procedure(sub1), pointer :: pptr1
    procedure(sub2), pointer :: pptr2
    integer i
    i = 0
    pptr1 => sub1
    call pptr1 (i)
    pptr1 => sub2
    call pptr1 (i)
    pptr2 => sub1
    call pptr2 (i)
    pptr2 => sub2
    call pptr2 (i)
    if (i .ne. 22) call abort
  end subroutine test
  subroutine sub2(arg)
    integer arg
    arg = arg + 10
  end subroutine sub2
end program main
