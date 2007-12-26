! { dg-do run }
! PR34560 I/O internal read: END expected, but no failure
program main
  character(len=2) :: line
  character(len=1) :: a(3)
  a = "x"
  line = 'ab'
  read (line,'(A)',END=99) a
  call abort
  99 continue 
  if (any(a /= ['a','x','x'])) call abort
end program main
