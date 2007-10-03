! { dg-do compile }
! Tests a number of error messages relating to derived type objects
! in common blocks.  Originally due to PR 33198

subroutine one
type a
   sequence
   integer :: i = 1
end type a
type(a) :: t ! { dg-error "Derived type variable .t. in COMMON at ... may not have default initializer" }
common /c/ t
end

subroutine first
type a
   integer :: i
   integer :: j
end type a
type(a) :: t  ! { dg-error "Derived type variable .t. in COMMON at ... has neither the SEQUENCE nor the BIND.C. attribute" }
common /c/ t
end

subroutine prime
type a
   sequence
   integer, allocatable :: i(:)
   integer :: j
end type a
type(a) :: t  ! { dg-error "Derived type variable .t. in COMMON at ... has an ultimate component that is allocatable" }
common /c/ t
end

subroutine source
parameter(x=0.) ! { dg-error "COMMON block .x. at ... is used as PARAMETER at ..." }
common /x/ i  ! { dg-error "COMMON block .x. at ... is used as PARAMETER at ..." }
intrinsic sin
common /sin/ j ! { dg-error "COMMON block .sin. at ... is also an intrinsic procedure" }
end subroutine source
