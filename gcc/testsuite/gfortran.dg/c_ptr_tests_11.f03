! { dg-do compile }
! Verify that initialization of c_ptr components works.
module fgsl
  use, intrinsic :: iso_c_binding
  implicit none
  type, public :: fgsl_matrix
     private
     type(c_ptr) :: gsl_matrix = c_null_ptr
  end type fgsl_matrix
  type, public :: fgsl_multifit_fdfsolver
     private
     type(c_ptr) :: gsl_multifit_fdfsolver = c_null_ptr
  end type fgsl_multifit_fdfsolver
interface
  function gsl_multifit_fdfsolver_jac(s) bind(c)
    import :: c_ptr
    type(c_ptr), value :: s
    type(c_ptr) :: gsl_multifit_fdfsolver_jac
  end function gsl_multifit_fdfsolver_jac
end interface
contains
  function fgsl_multifit_fdfsolver_jac(s)
    type(fgsl_multifit_fdfsolver), intent(in) :: s
    type(fgsl_matrix) :: fgsl_multifit_fdfsolver_jac
    fgsl_multifit_fdfsolver_jac%gsl_matrix = &
         gsl_multifit_fdfsolver_jac(s%gsl_multifit_fdfsolver)
  end function fgsl_multifit_fdfsolver_jac
end module fgsl

module m
  use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr
  implicit none
  type t
    type(c_ptr) :: matrix  = c_null_ptr
  end type t
contains
  subroutine func(a)
    type(t), intent(out) :: a
  end subroutine func
end module m
