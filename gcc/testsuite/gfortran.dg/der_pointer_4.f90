! { dg-do compile }
! PR 24426
! Pointer-components of derived type with initialized components
module crash
  implicit none
  type foo
    integer :: i = 0
    type (foo), pointer :: next
  end type foo
  type (foo), save :: bar
end module crash
