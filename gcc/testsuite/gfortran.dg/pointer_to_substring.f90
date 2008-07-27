! { dg-do compile }
! PR36724 - ICE on pointer to substring
! testcase contributed by Loukas Peristeras.

  character(LEN=132), target :: line
  character(LEN=1), pointer :: t

  read(*,'(A)') line
  t=>line(1:1)
end
