! { dg-do run }
! PR69651 Usage of unitialized pointer io/list_read.c 
! Note: The uninitialized pointer was not the cause of the problem
!       observed with this test case. The problem was mishandling '!'
!       See also test case read_bang4.f90.
program test
  implicit none
  integer :: i, j, ios
  real ::  r, s
  complex :: c, d
  character(20) :: str1, str2
  
  i = -5
  j = -6
  r = -3.14
  s = -2.71
  c = (-1.1,-2.2)
  d = (-3.3,-4.4)
  str1 = "candy"
  str2 = "peppermint"
  open(15, status='scratch')
  write(15,*) "10  1!2"
  write(15,*) "  23.5! 34.5"
  write(15,*) "  (67.50,69.25)  (51.25,87.75)!"
  write(15,*) "  'abcdefgh!' '  !klmnopq!'"
  rewind(15)
  read(15,*,iostat=ios) i, j
  if (ios.ne.5010) call abort
  read(15,*,iostat=ios) r, s
  if (ios.ne.5010) call abort
  read(15,*,iostat=ios) c, d
  if (ios.ne.5010) call abort
  read(15,*,iostat=ios) str1, str2
  if (ios.ne.0) call abort
  if (str1.ne."abcdefgh!") print *, str1
  if (str2.ne."  !klmnopq!") print *, str2
  close(15)
end program
