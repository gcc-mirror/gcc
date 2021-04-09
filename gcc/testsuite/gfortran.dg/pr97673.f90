! { dg-do compile }
! { dg-options "-O3 -fno-early-inlining --param large-stack-frame=4000" }

subroutine sub3noiso(a, b)
  use iso_c_binding
  implicit none
  character(len=1,kind=c_char) :: a(*), b
  character(len=1,kind=c_char):: x,z
  integer(c_int) :: y
  value :: b
  print *, a(1:2), b
entry sub3noisoEntry(x,y,z)
  x = 'd'
end subroutine sub3noiso
