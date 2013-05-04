! { dg-do compile }
! { dg-options "-std=f2008ts" }
!
! Contributed by Reinhold Bader
! 
use iso_c_binding
type, bind(C) :: cstruct
  integer :: i
end type
interface
     subroutine psub(this, that) bind(c, name='Psub')
       import :: c_float, cstruct
       real(c_float), pointer  :: this(:)
       type(cstruct), allocatable  :: that(:)
     end subroutine psub
  end interface
end
