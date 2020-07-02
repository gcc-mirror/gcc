! { dg-do compile }
! PR fortran/93337 - ICE in gfc_dt_upper_string, at fortran/module.c:441

program p
  type t
     character(:), allocatable :: a
  end type t
  class(t) :: x ! { dg-error "must be dummy, allocatable or pointer" }
  x = x         ! { dg-error "must not be polymorphic in intrinsic assignment" }
end
