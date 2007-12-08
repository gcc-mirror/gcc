! PR debug/33739
! { dg-do compile }
! { dg-options "-g3" }
subroutine a
include 'include_1.inc'
end subroutine a
subroutine b
include 'include_1.inc'
end subroutine b
