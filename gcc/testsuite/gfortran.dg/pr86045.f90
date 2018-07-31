! { dg-do compile }
program p
   logical :: a(2) = (mod([2,3],0) == 0)     ! { dg-error "shall not be zero" }
   integer :: b = count(mod([2,3],0) == 0)   ! { dg-error "shall not be zero" }
   integer :: c = all(mod([2,3],0) == 0)     ! { dg-error "shall not be zero" }
   integer :: d = any(mod([2,3],0) == 0)     ! { dg-error "shall not be zero" }
end
