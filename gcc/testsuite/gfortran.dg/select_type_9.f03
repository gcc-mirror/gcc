! { dg-do compile }
!
! PR 42053: [OOP] SELECT TYPE: reject duplicate CLASS IS blocks
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

 type :: t
  integer :: i
 end type

 CLASS(t),pointer :: x

 select type (x)
 class is (t)
  print *,"a"
 class is (t)  ! { dg-error "Double CLASS IS block" }
  print *,"b"
 end select

end
