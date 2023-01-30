! { dg-do compile }
! PR fortran/108501 - ICE in get_expr_storage_size
! Contributed by G.Steinmetz

program p
  real, parameter :: n = 2
  real :: a(1,(n),2) ! { dg-error "must be of INTEGER type" }
  call s(a(:,:,1))
end
subroutine s(x)
  real :: x(2)
end

! { dg-prune-output "must have constant shape" }
