! { dg-do compile }
!
! Contributed by Reinhold Bader
!
program assumed_shape_01
  use, intrinsic :: iso_c_binding
  implicit none
  type, bind(c) :: cstruct
     integer(c_int) :: i
     real(c_float) :: r(2)
  end type cstruct
  interface
     subroutine psub(this, that) bind(c, name='Psub')
       import :: c_float, cstruct
       real(c_float) :: this(:,:)
       type(cstruct) :: that(:)
     end subroutine psub
  end interface

  real(c_float) :: t(3,7)
  type(cstruct), pointer :: u(:)

! The following is VALID Fortran 2008 but NOT YET supported 
  allocate(u, source=[cstruct( 4, [1.1,2.2] ) ]) 
  call psub(t, u)
  deallocate (u)

end program assumed_shape_01
