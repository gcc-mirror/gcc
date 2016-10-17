! { dg-do compile }
! { dg-options "-std=f2008" }
subroutine a1
  integer, parameter :: i = -666
  stop i
end subroutine a1

subroutine a2
  stop -666
end subroutine a2

subroutine a3
  integer, parameter :: i = 123456
  stop i
end subroutine a3

subroutine a4
  stop 123456
end subroutine a4

subroutine a5
  stop merge(667,668,.true.) 
end subroutine a5
