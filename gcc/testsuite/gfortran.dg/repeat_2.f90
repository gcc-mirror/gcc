! REPEAT intrinsic
!
! { dg-do run }
subroutine foo(i, j, s, t)
  implicit none
  integer, intent(in) :: i, j
  character(len=i), intent(in) :: s
  character(len=i*j), intent(in) :: t

  if (repeat(s,j) /= t) call abort
  call bar(j,s,t)
end subroutine foo

subroutine bar(j, s, t)
  implicit none
  integer, intent(in) :: j
  character(len=*), intent(in) :: s
  character(len=len(s)*j), intent(in) :: t

  if (repeat(s,j) /= t) call abort
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

  if (repeat(t0, 0) /= "") call abort
  if (repeat(t1, 0) /= "") call abort
  if (repeat(t2, 0) /= "") call abort
  if (repeat(t0, 1) /= "") call abort
  if (repeat(t1, 1) /= "a") call abort
  if (repeat(t2, 1) /= "ab") call abort
  if (repeat(t0, 2) /= "") call abort
  if (repeat(t1, 2) /= "aa") call abort
  if (repeat(t2, 2) /= "abab") call abort

  if (repeat(s0, 0) /= "") call abort
  if (repeat(s1, 0) /= "") call abort
  if (repeat(s2, 0) /= "") call abort
  if (repeat(s0, 1) /= "") call abort
  if (repeat(s1, 1) /= "a") call abort
  if (repeat(s2, 1) /= "ab") call abort
  if (repeat(s0, 2) /= "") call abort
  if (repeat(s1, 2) /= "aa") call abort
  if (repeat(s2, 2) /= "abab") call abort

  i = 0
  if (repeat(t0, i) /= "") call abort
  if (repeat(t1, i) /= "") call abort
  if (repeat(t2, i) /= "") call abort
  i = 1
  if (repeat(t0, i) /= "") call abort
  if (repeat(t1, i) /= "a") call abort
  if (repeat(t2, i) /= "ab") call abort
  i = 2
  if (repeat(t0, i) /= "") call abort
  if (repeat(t1, i) /= "aa") call abort
  if (repeat(t2, i) /= "abab") call abort

  i = 0
  if (repeat(s0, i) /= "") call abort
  if (repeat(s1, i) /= "") call abort
  if (repeat(s2, i) /= "") call abort
  i = 1
  if (repeat(s0, i) /= "") call abort
  if (repeat(s1, i) /= "a") call abort
  if (repeat(s2, i) /= "ab") call abort
  i = 2
  if (repeat(s0, i) /= "") call abort
  if (repeat(s1, i) /= "aa") call abort
  if (repeat(s2, i) /= "abab") call abort

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
