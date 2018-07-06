! REPEAT intrinsic
!
! { dg-do run }
subroutine foo(i, j, s, t)
  implicit none
  integer, intent(in) :: i, j
  character(len=i), intent(in) :: s
  character(len=i*j), intent(in) :: t

  if (repeat(s,j) /= t) STOP 1
  call bar(j,s,t)
end subroutine foo

subroutine bar(j, s, t)
  implicit none
  integer, intent(in) :: j
  character(len=*), intent(in) :: s
  character(len=len(s)*j), intent(in) :: t

  if (repeat(s,j) /= t) STOP 2
end subroutine bar

program test
  implicit none
  character(len=0), parameter :: s0 = "" 
  character(len=1), parameter :: s1 = "a"
  character(len=2), parameter :: s2 = "ab"
  character(len=0) :: t0 
  character(len=1) :: t1
  character(len=2) :: t2
  integer :: i

  t0 = ""
  t1 = "a"
  t2 = "ab"

  if (repeat(t0, 0) /= "") STOP 3
  if (repeat(t1, 0) /= "") STOP 4
  if (repeat(t2, 0) /= "") STOP 5
  if (repeat(t0, 1) /= "") STOP 6
  if (repeat(t1, 1) /= "a") STOP 7
  if (repeat(t2, 1) /= "ab") STOP 8
  if (repeat(t0, 2) /= "") STOP 9
  if (repeat(t1, 2) /= "aa") STOP 10
  if (repeat(t2, 2) /= "abab") STOP 11

  if (repeat(s0, 0) /= "") STOP 12
  if (repeat(s1, 0) /= "") STOP 13
  if (repeat(s2, 0) /= "") STOP 14
  if (repeat(s0, 1) /= "") STOP 15
  if (repeat(s1, 1) /= "a") STOP 16
  if (repeat(s2, 1) /= "ab") STOP 17
  if (repeat(s0, 2) /= "") STOP 18
  if (repeat(s1, 2) /= "aa") STOP 19
  if (repeat(s2, 2) /= "abab") STOP 20

  i = 0
  if (repeat(t0, i) /= "") STOP 21
  if (repeat(t1, i) /= "") STOP 22
  if (repeat(t2, i) /= "") STOP 23
  i = 1
  if (repeat(t0, i) /= "") STOP 24
  if (repeat(t1, i) /= "a") STOP 25
  if (repeat(t2, i) /= "ab") STOP 26
  i = 2
  if (repeat(t0, i) /= "") STOP 27
  if (repeat(t1, i) /= "aa") STOP 28
  if (repeat(t2, i) /= "abab") STOP 29

  i = 0
  if (repeat(s0, i) /= "") STOP 30
  if (repeat(s1, i) /= "") STOP 31
  if (repeat(s2, i) /= "") STOP 32
  i = 1
  if (repeat(s0, i) /= "") STOP 33
  if (repeat(s1, i) /= "a") STOP 34
  if (repeat(s2, i) /= "ab") STOP 35
  i = 2
  if (repeat(s0, i) /= "") STOP 36
  if (repeat(s1, i) /= "aa") STOP 37
  if (repeat(s2, i) /= "abab") STOP 38

  call foo(0,0,"","")
  call foo(0,1,"","")
  call foo(0,2,"","")
  call foo(1,0,"a","")
  call foo(1,1,"a","a")
  call foo(1,2,"a","aa")
  call foo(2,0,"ab","")
  call foo(2,1,"ab","ab")
  call foo(2,2,"ab","abab")
end program test
