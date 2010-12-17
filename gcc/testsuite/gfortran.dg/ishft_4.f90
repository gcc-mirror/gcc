! We want to check that ISHFT evaluates its arguments only once
!
! { dg-do run }
! { dg-options "-fdump-tree-original" }

program test

  if (ishft (foo(), 2) /= 4) call abort
  if (ishft (foo(), -1) /= 1) call abort
  if (ishft (1, foo()) /= 8) call abort
  if (ishft (16, -foo()) /= 1) call abort

  if (ishftc (bar(), 2) /= 4) call abort
  if (ishftc (bar(), -1) /= 1) call abort
  if (ishftc (1, bar()) /= 8) call abort
  if (ishftc (16, -bar()) /= 1) call abort

contains
  
  integer function foo ()
    integer, save :: i = 0
    i = i + 1
    foo = i
  end function

  integer function bar ()
    integer, save :: i = 0
    i = i + 1
    bar = i
  end function

end program

! The regexp "foo ()" should be seen once in the dump:
!   -- once in the function definition itself
!   -- plus as many times as the function is called
!
! { dg-final { scan-tree-dump-times "foo *\\\(\\\)" 5 "original" } }
! { dg-final { scan-tree-dump-times "bar *\\\(\\\)" 5 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
