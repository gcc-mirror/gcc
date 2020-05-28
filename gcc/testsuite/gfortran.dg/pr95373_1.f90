! { dg-do compile }
! { dg-options "-std=f95" }
! PR fortran/95373 - ICE in build_reference_type, at tree.c:7942

subroutine s (x)
  complex, parameter :: z = 3
  real(z% kind)      :: x       ! { dg-error "nonderived-type variable" }
  type t
     real    :: kind
     logical :: re
  end type t
  type(t) :: b
  print *, b% kind, b% re
  print *, z% re                ! { dg-error "nonderived-type variable" }
end
