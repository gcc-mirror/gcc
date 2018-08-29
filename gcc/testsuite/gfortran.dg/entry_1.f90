! { dg-do run }
! Test alternate entry points in a module procedure
! Also check that references to sibling entry points are resolved correctly.
module m
contains
subroutine indirecta (p)
  call p (3, 4)
end subroutine
subroutine indirectb (p)
  call p (5)
end subroutine

subroutine test1
  implicit none
  call indirecta (foo)
  call indirectb (bar)
end subroutine

subroutine foo(a, b)
  integer a, b
  logical, save :: was_foo = .false.
  if ((a .ne. 3) .or. (b .ne. 4)) STOP 1
  was_foo = .true.
entry bar(a)
  if (was_foo) then
    if ((a .ne. 3) .or. (b .ne. 4)) STOP 2
  else
    if (a .ne. 5) STOP 3
  end if
  was_foo = .false.
end subroutine

subroutine test2
  call foo (3, 4)
  call bar (5)
end subroutine
end module

program p
  use m
  call foo (3, 4)
  call bar (5)
  call test1 ()
  call test2 ()
end program
