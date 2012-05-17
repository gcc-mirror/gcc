! { dg-do run }
! { dg-additional-sources bind_c_usage_17_c.c }
!
! PR fortran/37201
!
! 
!
MODULE mod
  INTERFACE
    FUNCTION cdir() BIND(C,name="cdir") RESULT(r)
      USE iso_c_binding
      CHARACTER(kind=C_CHAR) :: r
    END FUNCTION
  END INTERFACE
END MODULE

PROGRAM test
  USE mod
  integer :: i = -43
  character(len=1) :: str1
  character(len=4) :: str4
  str1 = 'x'
  str4 = 'xyzz'
  str1 = cdir()
  if(str1 /= '/') call abort()
  str4 = cdir()
  if(str4 /= '/' .or. ichar(str4(2:2)) /= 32) call abort()
  i   = ICHAR(cdir())
  if (i /= 47) call abort()
  str4 = 'xyzz'
  WRITE(str4,'(a)') cdir()
  if(str4 /= '/' .or. ichar(str4(2:2)) /= 32) call abort()
  str4 = 'xyzz'
  WRITE(str4,'(i0)') ICHAR(cdir())
  if(str4 /= '47' .or. ichar(str4(3:3)) /= 32) call abort()
END PROGRAM
