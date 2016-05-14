! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Fortran/PR71047
!

module m
 implicit none

 type, abstract :: c_abstr
  integer :: i = 0
 end type c_abstr

 type, extends(c_abstr) :: t_a
  class(c_abstr), allocatable :: f
 end type t_a
 
 type, extends(c_abstr) :: t_b
 end type t_b

contains

 subroutine set(y,x)
  class(c_abstr), intent(in)  :: x
  type(t_a),      intent(out) :: y
   allocate( y%f , source=x )
 end subroutine set

end module m


program p
 use m
 implicit none

 type(t_a) :: res
 type(t_b) :: var

  call set( res , var )
  write(*,*) res%i

end program p

!
! Check to ensure the vtable is actually initialized.
!
! { dg-final { scan-tree-dump "t_a\\.\\d+\\.f\\._vptr =" "original" } }
!
