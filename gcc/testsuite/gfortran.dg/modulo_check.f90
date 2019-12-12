! { dg-do compile }
! Test checks on modulo with p == 0
program p
   logical :: a(2) = (modulo([2,3],0) == 0)     ! { dg-error "shall not be zero" }
   integer :: b = count(modulo([2,3],0) == 0)   ! { dg-error "shall not be zero" }
   integer :: c = all(modulo([2,3],0) == 0)     ! { dg-error "shall not be zero" }
   integer :: d = any(modulo([2,3],0) == 0)     ! { dg-error "shall not be zero" }
end program
