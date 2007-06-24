! { dg-do run }
! { dg-shouldfail "Runtime error format check" }
! PR32456 IO error message should show Unit/Filename
program test
  implicit none
  integer :: i
  open(99, status="scratch")
  read(99,*) i
end program
! { dg-output ".*(unit = 99, file = .*)" }
! { dg-output "Fortran runtime error: End of file" }
