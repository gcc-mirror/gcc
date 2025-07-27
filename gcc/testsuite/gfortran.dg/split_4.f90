! { dg-do run }
! { dg-shouldfail "Fortran runtime error" }

program b
  character(len=:), allocatable :: input
  character(len=2) :: set = ', '
  integer :: p
  input = " one,last example,"
  p = 0
  call split(input, set, p, .true.)
end program b
