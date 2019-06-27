! { dg-do compile }
program pr89782
  character(len=*),parameter :: VALUES(*)=[character(len=10) :: 'NaN','NAN','nan','Inf','INF','inf','Infinity']
  character(len=*),parameter :: VALUE='NaN'
  real(4) :: var
  do i=1,size(VALUES)
    read(VALUES(i),*) float ! { dg-error "character PARAMETER" }
    write(VALUES(i),*)float ! { dg-error "character PARAMETER" }
  enddo
  read(var,*)float    ! { dg-error "INTEGER expression or a CHARACTER" }
  read(VALUE,*)float  ! { dg-error "character PARAMETER" }
  write(VALUE,*)float ! { dg-error "character PARAMETER" }
end program pr89782
