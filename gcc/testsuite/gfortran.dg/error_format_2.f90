! { dg-do run }
! PR68987, this test case failed on a memory double free
program foo
  call s('(foo)')
end program
subroutine s(fmt)
  character (*) :: fmt
  character (1) :: c
  integer :: i
  write (c, fmt, iostat=i) 42
 ! print *, i
  if (i==0) call abort()
  write (c, fmt, err=100) 42
  call abort()
100 continue
end subroutine
