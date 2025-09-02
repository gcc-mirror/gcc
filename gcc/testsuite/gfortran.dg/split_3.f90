! { dg-do run }
! { dg-shouldfail "Fortran runtime error" }

program b
  character(len=:), allocatable :: input
  character(len=2) :: set = ', '
  integer :: p
  input = " one,last example,"
  p = -1
  call split(input, set, p)
end program b
