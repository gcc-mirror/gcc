! { dg-do compile }
! { dg-options "-std=f2008" }
!
! Check relaxed TS29113 constraints for procedures
! and c_f_*pointer argument checking for c_ptr/c_funptr.
!

use iso_c_binding
implicit none
type(c_ptr) :: cp
type(c_funptr) :: cfp

interface
  subroutine sub() bind(C)
  end subroutine sub
end interface
integer(c_int), pointer :: int
procedure(sub), pointer :: fsub

integer, external :: noCsub
procedure(integer), pointer :: fint

cp = c_funloc (sub) ! { dg-error "Can't convert TYPE.c_funptr. to TYPE.c_ptr." })
cfp = c_loc (int)   ! { dg-error "Can't convert TYPE.c_ptr. to TYPE.c_funptr." }

call c_f_pointer (cfp, int)     ! { dg-error "Argument CPTR at .1. to C_F_POINTER shall have the type TYPE.C_PTR." }
call c_f_procpointer (cp, fsub) ! { dg-error "Argument CPTR at .1. to C_F_PROCPOINTER shall have the type TYPE.C_FUNPTR." }

cfp = c_funloc (noCsub) ! { dg-error "Fortran 2018: Noninteroperable procedure at .1. to C_FUNLOC" }
call c_f_procpointer (cfp, fint) ! { dg-error "Fortran 2018: Noninteroperable procedure pointer at .1. to C_F_PROCPOINTER" }
end
