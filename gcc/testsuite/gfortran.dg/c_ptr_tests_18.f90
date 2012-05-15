! { dg-do compile }
!
! PR fortran/37829
! PR fortran/45190
!
! Contributed by Mat Cross
!
! Fix derived-type loading with ISO_BIND_C's C_PTR/C_FUNPTR.

MODULE NAG_J_TYPES
  USE ISO_C_BINDING, ONLY : C_PTR
  IMPLICIT NONE
  TYPE                            :: NAG_IMAGE
     INTEGER                      :: WIDTH, HEIGHT, PXFMT, NCHAN
     TYPE (C_PTR)                 :: PIXELS
  END TYPE NAG_IMAGE
END MODULE NAG_J_TYPES
program cfpointerstress
  use nag_j_types
  use iso_c_binding
  implicit none
  type(nag_image),pointer :: img
  type(C_PTR)             :: ptr
  real, pointer           :: r
  allocate(r)
  allocate(img)
  r = 12
  ptr = c_loc(img)
  write(*,*) 'C_ASSOCIATED =', C_ASSOCIATED(ptr)
  call c_f_pointer(ptr, img)
  write(*,*) 'ASSOCIATED =', associated(img)
  deallocate(r)
end program cfpointerstress
