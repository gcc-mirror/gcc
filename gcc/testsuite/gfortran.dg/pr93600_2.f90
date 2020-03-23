! { dg-do run }

program p
  integer, parameter :: a(0) = 0
  character(0), parameter :: b(0) = ''
  integer :: c
  if (a%kind.ne.kind(c)) stop 1
  if (b%len.ne.0) stop 2
end program

