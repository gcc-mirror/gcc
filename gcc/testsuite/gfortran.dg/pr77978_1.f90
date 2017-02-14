! { dg-do compile }
! { dg-options "-std=f95" }
subroutine a1
  integer, parameter :: i = -666
  stop i ! { dg-error "cannot be negative" }
end subroutine a1

subroutine a2
  stop -666 ! { dg-error "cannot be negative" }
end subroutine a2

subroutine a3
  integer, parameter :: i = 123456
  stop i ! { dg-error "too many digits" }
end subroutine a3

subroutine a4
  stop 123456 ! { dg-error "too many digits" }
end subroutine a4

!subroutine a5
!  stop merge(667,668,.true.) 
!end subroutine a5
