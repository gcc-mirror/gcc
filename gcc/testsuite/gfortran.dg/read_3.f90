! { dg-do run }
! PR80727 Crash of runtime gfortran library during integer transformation
! Note: before the patch this was giving an incorrect EOR error on READ.
program    gfortran_710_io_bug
  character  str*4
  integer(4) :: i4
  str =''
  i = 256
  write(str,fmt='(a)') i
  i = 0
  read ( unit=str(1:4), fmt='(a)' ) i4
  if (i4.ne.256) STOP 1
end  program  gfortran_710_io_bug 
