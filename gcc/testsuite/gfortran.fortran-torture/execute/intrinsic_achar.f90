! Program to test the ACHAR and IACHAR intrinsics
program intrinsic_achar
  integer i

  i = 32
  if (achar(i) .ne. " ") call abort
  i = iachar("A")
  if ((i .ne. 65) .or. char(i) .ne. "A") call abort
end program
