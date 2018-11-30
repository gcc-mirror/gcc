! { dg-do compile }
! { dg-options "-fopenmp -fdec-include" }
subroutine foo
  implicit none
!$  incl& ! comment1
!$ &u&
!$       &de           &     ! comment2
!$ 'include&
  &_1.inc'
  i = 1
end subroutine foo
subroutine bar
  implicit none
!$ include &

! comment3

!$ "include_1.inc"
  i = 1
end subroutine bar
subroutine baz
  implicit none
!$                                  include&
!$ &'include_1.&
!$ &inc'
  i = 1
end subroutine baz
subroutine qux
  implicit none
!$  include '&
include_1.inc'
end subroutine qux
