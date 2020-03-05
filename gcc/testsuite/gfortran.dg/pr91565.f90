! { dg-do compile }
! PR fortran/91565
! Contributed by Gerhard Steinmetz
program p
   integer, parameter :: a(2) = [2,2]              ! { dg-error "\(1\)" }
   print *, reshape([1,2,3,4,5,6], [2,3], order=a) ! { dg-error "not a permutation" }
end

subroutine foo
   integer, parameter :: a(1) = 1                  ! { dg-error "\(1\)" }
   print *, reshape([1,2,3,4,5,6], [2,3], order=a) ! { dg-error "are different" }
end

subroutine bar
   integer, parameter :: a(1,2) = 1                ! { dg-error "\(1\)" }
   print *, reshape([1,2,3,4,5,6], [2,3], order=a) ! { dg-error "are different" }
end
