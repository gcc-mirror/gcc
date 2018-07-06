! Program to test the ACHAR and IACHAR intrinsics
program intrinsic_achar
  integer i

  i = 32
  if (achar(i) .ne. " ") STOP 1
  i = iachar("A")
  if ((i .ne. 65) .or. char(i) .ne. "A") STOP 2
end program
