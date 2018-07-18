! { dg-do compile }
!
! PR fortran/46244 (comments 7 to 9)
!
! gfortran accepted CLASS in bind(C) and SEQUENCE types
!
type :: t
  integer :: i
end type t

type t2
  sequence
  class(t), pointer :: x ! { dg-error "Polymorphic component x at .1. in SEQUENCE or BIND" }
end type t2

type, bind(C):: t3
  class(t), pointer :: y
  ! { dg-error "Polymorphic component y at .1. in SEQUENCE or BIND" "" { target *-*-* } .-1 }
end type t3
end
