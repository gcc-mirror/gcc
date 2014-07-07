! { dg-do run }
! PR61640 KIND=4 Character Array Internal Unit Read Fail
program read_internal
  integer :: x(9),i
  integer :: y(9)
  character(kind=4,len=30), dimension(3) :: source

  y = reshape ((/ 1,1,-1,1,-1,1,-1,1,1 /), shape(x))
  source=[4_"  1   1  -1",4_"  1  -1   1",4_" -1   1   1"]
  !print *, (trim(source(i)), i=1,3)
  read(source,*) (x(i), i=1,9) ! This read fails for KIND=4 character
  if (any(x /= y )) call abort
end program read_internal
