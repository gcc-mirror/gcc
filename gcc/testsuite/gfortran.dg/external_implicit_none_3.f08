! { dg-do compile }
! { dg-options "-std=f2018" }
! Tests fix for PR100972 - Fails to warn about missing EXTERNAL attribute
! Contributed by Gerhard Steinmetz

program p
   implicit none (external)
   real, external :: f
   real :: a
   real :: b
   integer :: i
   character :: c
   a = f() ! OK
   b = g() ! { dg-error "Missing explicit declaration with EXTERNAL attribute" }
   i = h() ! { dg-error "Missing explicit declaration with EXTERNAL attribute" }
   c = j() ! { dg-error "Missing explicit declaration with EXTERNAL attribute" }
end
