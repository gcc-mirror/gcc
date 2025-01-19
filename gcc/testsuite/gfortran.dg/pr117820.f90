! { dg-do run }
! See pr117820, original testcase provided by Malcolm Cohen.
program test
  integer(8) :: x
  character(80) :: output
  output = "garbage"
  x = ibset (0_8, 63)
  write(output, '("<",B64.0,">")') x
  if (output .ne. "<1000000000000000000000000000000000000000000000000000000000000000>") stop 1
end program
