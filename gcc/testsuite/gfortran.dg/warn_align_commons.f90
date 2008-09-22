! { dg-do compile }
! { dg-options "-Wno-align-commons" }

! PR fortran/37486
!
! Test for -Wno-align-commons.
!
! Contributed by Janus Weil <janus@gcc.gnu.org>.

implicit none
integer(kind=4) :: n
real(kind=8) :: p
common /foo/ n,p   ! { dg-bogus "padding" }
end  
