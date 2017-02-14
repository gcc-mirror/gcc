! { dg-do compile }
! PR fortran/65173
program min_obj
   implicit none
   integer, parameter :: a = 128
   type :: param_t
      integer :: n= 0
      real*8, dimension(256), allocatable :: x           ! { dg-error "must have a deferred shape" }
      real*8, dimension(2,256), allocatable :: bounds    ! { dg-error "must have a deferred shape" }
      character(a), dimension(256), allocatable :: names ! { dg-error "must have a deferred shape" }
   end type param_t
   contains
      subroutine extrace_params_from_section ( )
         character(*), dimension(), parameter :: &      ! { dg-error "expression in array specification" }
         & char_params = ['element', 'parametrization']
      end subroutine extrace_params_from_section
end program min_obj
