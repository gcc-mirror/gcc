! { dg-do run }
program b
  character(len=:), allocatable :: input
  character(len=2) :: set = ', '
  integer :: p
  input = " one,last example,"
  p = 0

  call split(input, set, p)
  if (p /= 1) STOP 1
  call split(input, set, p)
  if (p /= 5) STOP 2
  call split(input, set, p)
  if (p /= 10) STOP 3
  call split(input, set, p)
  if (p /= 18) STOP 4
  call split(input, set, p)
  if (p /= 19) STOP 5

  call split(input, set, p, .true.)
  if (p /= 18) STOP 6
  call split(input, set, p, .true.)
  if (p /= 10) STOP 7
  call split(input, set, p, .true.)
  if (p /= 5) STOP 8
  call split(input, set, p, .true.)
  if (p /= 1) STOP 9
end program b
