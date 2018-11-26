! { dg-do compile }
! { dg-options "-fdec" }
subroutine foo
  implicit none
  incl& ! comment1
&u&
       &de           &     ! comment2
'include&
  &_10.inc'
  i = 1
end subroutine foo
subroutine bar
  implicit none
include &

! comment3

"include_10.inc"
  i = 1
end subroutine bar
subroutine baz
  implicit none
                                  include&
&'include_10.&
&inc'
  i = 1
end subroutine baz
subroutine qux
  implicit none
  include '&
include_10.inc'
end subroutine qux
subroutine quux
  implicit none
  include &
  &'include_10.inc'
  i = 1
end subroutine quux
subroutine quuz
  implicit none
  include &
  &"include_10.inc"
  i = 1
end subroutine quuz
