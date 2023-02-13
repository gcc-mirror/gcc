! { dg-do compile }
! { dg-options "-O2 -ffrontend-optimize" }
! PR fortran/108502 - ICE in gfc_check_dependency
! Contributed by G.Steinmetz

integer function n()
  integer :: a(1)
  a = [1] / 0
end
program p
  integer :: b = n() ! { dg-error "must be an intrinsic function" }
end
