! { dg-do compile }
! PR fortran/85779
class(t) function f()   ! { dg-error "must be dummy, allocatable or pointer" }
   type f               ! { dg-error "already has a basic type" }
   end type             ! { dg-error "END FUNCTION statement" }
end

