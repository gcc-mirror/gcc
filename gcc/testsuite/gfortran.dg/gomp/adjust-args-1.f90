! Test parsing of OMP clause adjust_args
! { dg-do compile }

module main
   use iso_c_binding, only: c_ptr, c_funptr
   implicit none
   integer :: b
   interface
      integer function f0 (a)
         import c_ptr
         type(c_ptr), intent(inout) :: a
      end function
      integer function g (a)
         import c_ptr
         type(c_ptr), intent(inout) :: a
      end function
      integer function f1 (i)
         integer, intent(in) :: i
      end function

      integer function f3 (a)
         import c_ptr
         type(c_ptr), intent(inout) :: a
         !$omp declare variant (f1) match (construct={dispatch}) adjust_args (other: a) ! { dg-error "expected 'nothing' or 'need_device_ptr' at .1." }
      end function
      integer function f4 (a)
         import c_ptr
         type(c_ptr), intent(inout) :: a
         !$omp declare variant (f0) adjust_args (nothing: a) ! { dg-error "an 'adjust_args' clause at .1. can only be specified if the 'dispatch' selector of the construct selector set appears in the 'match' clause" }
      end function
      integer function f5 (i)
         integer, intent(inout) :: i
         !$omp declare variant (f1) match (construct={dispatch}) adjust_args () ! { dg-error "expected 'nothing' or 'need_device_ptr' at .1." }
      end function
      integer function f6 (i)
         integer, intent(inout) :: i
         !$omp declare variant (f1) match (construct={dispatch}) adjust_args (nothing) ! { dg-error "expected argument list at .1." }
      end function
      integer function f7 (i)
         integer, intent(inout) :: i
         !$omp declare variant (f1) match (construct={dispatch}) adjust_args (nothing:) ! { dg-error "expected argument list at .1." }
      end function

   end interface
end module
