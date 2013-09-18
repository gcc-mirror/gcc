! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/43366
!
! Invalid assignment to an allocatable polymorphic var.
!
type t
end type t
class(t), allocatable :: var

var = t() ! { dg-error "Fortran 2008: Assignment to an allocatable polymorphic variable" }
end
