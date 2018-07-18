! { dg-do run }
! { dg-options "-ffrontend-optimize" }
program main
  character (len=:), allocatable :: a
  a = 'a'
  if (len(a) /= 1) STOP 1
  a = '  '
  if (len(a) /= 2) STOP 2
end program main
