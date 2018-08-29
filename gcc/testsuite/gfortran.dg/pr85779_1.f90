! { dg-do compile }
! PR fortran/85779
type(t) function f() ! { dg-error "is not accessible" }
   type f            ! { dg-error "already has a basic type" }
   end type          ! { dg-error "END FUNCTION statement" }
end
