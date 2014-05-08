! { dg-do compile }
! { dg-options "-std=f2008" }
!
! Contributed by Reinhold Bader
! 
use iso_c_binding
type, bind(C) :: cstruct
  integer :: i
end type
interface
     subroutine psub(this) bind(c, name='Psub') ! { dg-error "TS 29113/TS 18508: Variable 'this' at .1. with POINTER attribute in procedure 'psub' with BIND.C." }
       import :: c_float, cstruct
       real(c_float), pointer  :: this(:)
     end subroutine psub
     subroutine psub2(that) bind(c, name='Psub2') ! { dg-error "TS 29113/TS 18508: Variable 'that' at .1. with ALLOCATABLE attribute in procedure 'psub2' with BIND.C." }
       import :: c_float, cstruct
       type(cstruct), allocatable  :: that(:)
     end subroutine psub2
  end interface
end
