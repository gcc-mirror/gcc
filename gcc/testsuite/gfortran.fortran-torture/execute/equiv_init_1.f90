! Program to test initialization of equivalence blocks.  PR13742.
! Some forms are not yet implemented.  These are indicated by !!$

subroutine test0s
  character*10 :: x = "abcdefghij" 
  character*10 :: y
  equivalence (x,y)

  character*10 :: xs(10) 
  character*10 :: ys(10)
  equivalence (xs,ys)
  data xs /10*"abcdefghij"/

  if (y.ne."abcdefghij") STOP 1
  if (ys(1).ne."abcdefghij") STOP 2
  if (ys(10).ne."abcdefghij") STOP 3
end
  
subroutine test0
  integer :: x = 123
  integer :: y
  equivalence (x,y)
  if (y.ne.123) STOP 4
end

subroutine test1
  integer :: a(3)
  integer :: x = 1
  integer :: y
  integer :: z = 3
  equivalence (a(1), x)
  equivalence (a(3), z)
  if (x.ne.1) STOP 5
  if (z.ne.3) STOP 6
  if (a(1).ne.1) STOP 7
  if (a(3).ne.3) STOP 8
end

subroutine test2
  integer :: x
  integer :: z
  integer :: a(3) = 123
  equivalence (a(1), x)
  equivalence (a(3), z)
  if (x.ne.123) STOP 9
  if (z.ne.123) STOP 10
end

subroutine test3
  integer :: x
!!$  integer :: y = 2
  integer :: z
  integer :: a(3)
  equivalence (a(1),x), (a(2),y), (a(3),z)
  data a(1) /1/, a(3) /3/
  if (x.ne.1) STOP 11
!!$  if (y.ne.2) STOP 12
  if (z.ne.3) STOP 13
end

subroutine test4
  integer a(2)
  integer b(2)
  integer c
  equivalence (a(2),b(1)), (b(2),c)
  data a/1,2/
  data c/3/
  if (b(1).ne.2) STOP 14
  if (b(2).ne.3) STOP 15
end

!!$subroutine test5
!!$  integer a(2)
!!$  integer b(2)
!!$  integer c
!!$  equivalence (a(2),b(1)), (b(2),c)
!!$  data a(1)/1/
!!$  data b(1)/2/
!!$  data c/3/
!!$  if (a(2).ne.2) STOP 16
!!$  if (b(2).ne.3) STOP 17
!!$  print *, "Passed test5"
!!$end
  
program main
  call test0s
  call test0
  call test1
  call test2
  call test3
  call test4
!!$  call test5
end

