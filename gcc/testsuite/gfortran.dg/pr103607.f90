! { dg-do compile }
! PR fortran/103607 - ICE in do_subscript, at fortran/frontend-passes.c:2927
! Contributed by G.Steinmetz

program p
  integer :: i, x(abs(2.)) ! { dg-error "must be of INTEGER type" }
  do i = 1, 2
     x(i) = 0
  end do
end

! { dg-prune-output "must have constant shape" }
