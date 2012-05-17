! { dg-do compile }
! PR 45338 - no ICE when cmp is not used explicitly.
! Test case by Simon Smart 
module test_mod
  implicit none
contains
  subroutine test_fn (cmp)
    interface operator(.myop.)
       pure function cmp (a, b) result(ret)
         integer, intent(in) :: a, b
         logical ret
       end function cmp
    end interface
    integer :: a, b
    print*, a .myop. b
  end subroutine test_fn
end module test_mod
