! { dg-do run }
! PR 15234
! tests for passing arrays of assumed length characters
program strarray_6
character(5), dimension(:), allocatable :: c
n = 3
allocate(c(-1:n-2))
c = "BLUBB"
call foo(c)
call bar(c,n)
deallocate(c)
contains
subroutine foo(x)
  character (len = *), dimension(:) :: x
  if (any (x .ne. "BLUBB")) CALL abort()
end subroutine foo
end

subroutine bar(x,n)
  character (len = *), dimension(n) :: x
  if (any (x .ne. "BLUBB")) CALL abort()
end subroutine bar
