! { dg-do compile }
! PR fortran/50540
!
  implicit none
  integer i,dest(10)
  forall (i=2:ix)  dest(i)=i ! { dg-error "has no IMPLICIT type" }
end                          ! { dg-error "Cannot convert UNKNOWN to INTEGER" "" { target "*-*-*" } .-1 }
