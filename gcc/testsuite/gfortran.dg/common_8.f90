! { dg-do compile }
!
! PR fortran/25062
!
! F2003: 16.2.1
! "A name that identifies a common block in a scoping unit shall not be used 
!  to identify a constant or an intrinsic procedure in that scoping unit."
!
subroutine try
 implicit none
 COMMON /s/ J
 COMMON /bar/ I
 INTEGER I, J
 real s, x
 s(x)=sin(x)
 print *, s(5.0)
 call bar()
contains
 subroutine bar
   print *, 'Hello world'
 end subroutine bar

end subroutine try

program test
 implicit none
 COMMON /abs/ J ! { dg-error "is also an intrinsic procedure" }
 intrinsic :: abs
 INTEGER J
 external try
 call try
end program test
