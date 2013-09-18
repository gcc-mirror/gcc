! { dg-do compile }
! { dg-options "-fno-realloc-lhs" }
!
! PR fortran/43366
!
! Invalid assignment to an allocatable polymorphic var.
!
type t
end type t
class(t), allocatable :: var

var = t() ! { dg-error "Assignment to an allocatable polymorphic variable at .1. requires -frealloc-lhs" }
end
