! { dg-do compile }
! PR fortran/85779
type(t) function f() result(z)   ! { dg-error "is not accessible" }
   type z                        ! { dg-error "already has a basic type" }
   end type                      ! { dg-error "END FUNCTION statement" }
end

