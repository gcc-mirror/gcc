! { dg-do compile }
! PR fortran/50540
!
  implicit none
  integer i,dest(10)
  forall (i=2:ix)  dest(i)=i ! { dg-error "has no IMPLICIT type" }
end

! { dg-excess-errors "Can't convert UNKNOWN to INTEGER" }
