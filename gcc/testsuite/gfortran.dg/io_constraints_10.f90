! { dg-do compile }
! { dg-options "-std=f95" }
!
! PR fortran/52335
!

integer :: lun
character(len=20) :: str

! VALID Fortran 95:
open(unit=lun,file=str,delim='apostrophe',status='old')
inquire(lun, delim=str)

! Fortran 2003:
write(*,*, delim='apostrophe') 'a' ! { dg-error "Fortran 2003: DELIM= at .1. not allowed in Fortran 95" }
end
