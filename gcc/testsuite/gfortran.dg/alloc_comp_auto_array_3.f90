! { dg-do compile }
! { dg-options "-O0 -fdump-tree-original" }
!
! Test the fix for PR66082. The original problem was with the first
! call foo_1d.
!
! Reported by Damian Rouson  <damian@sourceryinstitute.org>
!
  type foo_t
    real, allocatable :: bigarr
  end type
  block
    type(foo_t) :: foo
    allocate(foo%bigarr)
    call foo_1d (1,[foo]) ! wasy lost
    call foo_1d (1,bar_1d()) ! Check that this is OK
  end block
contains
  subroutine foo_1d (n,foo)
    integer n
    type(foo_t) :: foo(n)
  end subroutine
  function bar_1d () result (array)
    type(foo_t) :: array(1)
    allocate (array(1)%bigarr)
  end function
end
! { dg-final { scan-tree-dump-times "builtin_malloc" 3 "original" } }
! { dg-final { scan-tree-dump-times "builtin_free" 3 "original" } }
! { dg-final { scan-tree-dump-times "while \\(1\\)" 4 "original" } }
