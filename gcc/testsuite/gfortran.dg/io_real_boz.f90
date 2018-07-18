! { dg-do run }
! Test reading/writing of integer, real and character BOZ
! non-integer BOZ are not valid in standard Fortran, however.
! PR fortran/29625
program real_boz
  implicit none
  integer(4)        :: i,i2
  real(4)           :: r,r2
  complex(4)        :: z,z2
  character         :: c,c2
  character(len=100) :: str,fmt

  i = 43
  r = 325.56
  z = cmplx(14.456, 345342.456)
  c ='g'

  write(str,'(b0)') i
  write(fmt,'(a,i0,a)') '(b',len_trim(str),')'
  read(str,fmt) i2
  if(i /= i2) STOP 1

  write(str,'(o0)') i
  write(fmt,'(a,i0,a)') '(o',len_trim(str),')'
  read(str,fmt) i2
  if(i /= i2) STOP 2

  write(str,'(z0)') i
  write(fmt,'(a,i0,a)') '(z',len_trim(str),')'
  read(str,fmt) i2
  if(i /= i2) STOP 3


  write(str,'(b0)') r
  write(fmt,'(a,i0,a)') '(b',len_trim(str),')'
  read(str,fmt) r2
  if(r /= r2) STOP 4
  
  write(str,'(o0)') r
  write(fmt,'(a,i0,a)') '(o',len_trim(str),')'
  read(str,fmt) r2
  if(r /= r2) STOP 5

  write(str,'(z0)') r
  write(fmt,'(a,i0,a)') '(z',len_trim(str),')'
  read(str,fmt) r2
  if(r /= r2) STOP 6


  write(str,'(b0)') c
  write(fmt,'(a,i0,a)') '(b',len_trim(str),')'
  read(str,fmt) c2
  if(c /= c2) STOP 7

  write(str,'(o0)') c
  write(fmt,'(a,i0,a)') '(o',len_trim(str),')'
  read(str,fmt) c2
  if(c /= c2) STOP 8

  write(str,'(z0)') c
  write(fmt,'(a,i0,a)') '(z',len_trim(str),')'
  read(str,fmt) c2
  if(c /= c2) STOP 9

end program real_boz
  
