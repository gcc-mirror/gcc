! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/43366
!
! Invalid assignment to an allocatable polymorphic var.
!
type t
end type t
class(t), allocatable :: caf[:]

caf = t() ! { dg-error "Assignment to polymorphic coarray at .1. is not permitted" }
end
