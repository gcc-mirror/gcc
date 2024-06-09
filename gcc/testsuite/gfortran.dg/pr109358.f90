! { dg-do run }
! PR109358, test that tabs during stream io are correct.
program tabs
  implicit none
  integer :: fd
  character(64) :: line
  open(newunit=fd, file="otabs.txt", form="formatted", access="stream")
  write(fd, "(i4, t40, i4, t20, i5.5)") 1234, 5555, 67890
  close(fd)
  open(newunit=fd, file="otabs.txt", form="formatted")
  read(fd,"(a)") line
  close(fd, status='delete')
  if (line .ne. "1234               67890               5555") stop 10
end program tabs
