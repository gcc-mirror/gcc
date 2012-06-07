! { dg-do run }
! { dg-options "-ffrontend-optimize" }
program main
  character (len=:), allocatable :: a
  a = 'a'
  if (len(a) /= 1) call abort
  a = '  '
  if (len(a) /= 2) call abort
end program main
