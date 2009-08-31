! { dg-do compile }
! { dg-options "-std=f95" }
!
! PR 40940: [F03] CLASS statement
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

 type :: t
  integer :: comp
 end type

 class(t), pointer :: cl  ! { dg-error "CLASS statement" }

end

