! { dg-do compile }
!
! PR fortran/38665, in which checking for host association
! was wrongly trying to substitute mod_symmon(mult) with
! mod_sympoly(mult) in the user operator expression on line
! 43.
!
! Contributed by Thomas Koenig <tkoenig@gcc.gnu.org>
!
module mod_symmon
 implicit none

 public :: t_symmon, operator(*)
 private

 type t_symmon
   integer :: ierr = 0
 end type t_symmon

 interface operator(*)
   module procedure mult
 end interface

contains
 elemental function mult(m1,m2) result(m)
  type(t_symmon), intent(in) :: m1, m2
  type(t_symmon) :: m
  m%ierr = m1%ierr*m2%ierr
 end function mult
end module mod_symmon

module mod_sympoly
 use mod_symmon
 implicit none

 type t_sympol
   type(t_symmon), allocatable :: mons(:)
 end type t_sympol
contains

 elemental function mult(p1,p2) result(p)
  type(t_sympol), intent(in) :: p1,p2
  type(t_sympol) :: p
  p%mons= p1%mons*p2%mons
 end function
end module
! { dg-final { cleanup-modules "mod_symmon mod_sympoly" } }
