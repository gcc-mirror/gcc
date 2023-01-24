! { dg-do compile }
! PR fortran/108529 - ICE in transformational_result
! Contributed by G.Steinmetz

program p
  integer, parameter :: a(*,*) = reshape([1, 2, 3, 4], [2, 2])
  logical, parameter :: b(2,*) = a > 2     ! { dg-error "Assumed size" }
  logical, parameter :: c(*)   = all(b, 1) ! { dg-error "Bad shape" }
end
