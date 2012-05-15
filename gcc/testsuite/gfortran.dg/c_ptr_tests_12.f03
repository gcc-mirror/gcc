! { dg-do compile }
! Verify that initialization of c_ptr components works.  This is based on 
! code from fgsl: 
! http://www.lrz-muenchen.de/services/software/mathematik/gsl/fortran/
! and tests PR 33395.
module fgsl
  use, intrinsic :: iso_c_binding
  implicit none
!
!
! Kind and length parameters are default integer
!
  integer, parameter, public :: fgsl_double = c_double

!
! Types : Array support
!
  type, public :: fgsl_vector
     private
     type(c_ptr) :: gsl_vector = c_null_ptr
  end type fgsl_vector

contains
  function fgsl_vector_align(p_x, f_x)
    real(fgsl_double), pointer :: p_x(:)
    type(fgsl_vector) :: f_x
    integer :: fgsl_vector_align
    fgsl_vector_align = 4
  end function fgsl_vector_align
end module fgsl

module tmod
  use fgsl
  implicit none
contains
  subroutine expb_df() bind(c)
    type(fgsl_vector) :: f_x
    real(fgsl_double), pointer :: p_x(:)
    integer :: status
    status = fgsl_vector_align(p_x, f_x)
  end subroutine expb_df
end module tmod
