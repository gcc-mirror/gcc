! { dg-do run }
! PR 19568:  Don't read across end of line when the format is longer
!            than the line length and pad='yes' (default)
program main
  character(len=1) c1(10),c2(10)
  open(77,status='scratch')
  write(77,'(A)'), 'Line 1','Line 2','Line 3' ! { dg-warning "Comma before i/o item list" }
  rewind(77)
  read(77,'(10A1)'), c1 ! { dg-warning "Comma before i/o item list" }
  read(77,'(10A1)'), c2 ! { dg-warning "Comma before i/o item list" }
  if (c1(1) /= 'L' .or. c2(1) /= 'L') call abort
  close(77)
end program main
