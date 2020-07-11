! { dg-do compile }
! { dg-options "-fcoarray=single" }
! PR 93500 - this used to cause an ICE

program p
  integer :: a(min(2,0)/0) ! { dg-error "Division by zero" }
  integer, save :: c[min(2,0)/0,*] ! { dg-error "Division by zero|must have constant shape" }
  integer :: b = lbound(a) ! { dg-error "must be an array" }
  print *,lcobound(c)
end program p

subroutine s
  integer :: a(min(2,0)/0)  ! { dg-error "Division by zero" }
  integer, save :: c[min(2,0)/0,*] ! { dg-error "Division by zero" }
  integer :: b = lbound(a)
  print *,lcobound(c)
end subroutine s
