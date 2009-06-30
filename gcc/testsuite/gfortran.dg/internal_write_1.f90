! { dg-do run }
! { dg-shouldfail "End of file" }
program main
  character(len=20) :: line
  integer, dimension(4) :: n
  n = 1
  write(line,'(2I2)') n
end program main
! { dg-output "Fortran runtime error: End of file" }
