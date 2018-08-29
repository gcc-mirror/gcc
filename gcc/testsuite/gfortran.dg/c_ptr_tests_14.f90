! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/41298
!
! Check that c_null_ptr default initializer is really applied

module m
  use iso_c_binding
  type, public :: fgsl_file
     type(c_ptr)    :: gsl_file = c_null_ptr
     type(c_funptr) :: gsl_func = c_null_funptr
     type(c_ptr)    :: NIptr
     type(c_funptr) :: NIfunptr
  end type fgsl_file
contains
  subroutine sub(aaa,bbb)
    type(fgsl_file), intent(out)   :: aaa
    type(fgsl_file), intent(inout) :: bbb
  end subroutine
  subroutine proc() bind(C)
  end subroutine proc
end module m

program test
  use m
  implicit none
  type(fgsl_file) :: file, noreinit
  integer, target :: tgt

  call sub(file, noreinit)
  if(c_associated(file%gsl_file)) STOP 1
  if(c_associated(file%gsl_func)) STOP 2

  file%gsl_file = c_loc(tgt)
  file%gsl_func = c_funloc(proc)
  call sub(file, noreinit)
  if(c_associated(file%gsl_file)) STOP 3
  if(c_associated(file%gsl_func)) STOP 4
end program test

! { dg-final { scan-tree-dump-times "c_funptr.\[0-9\]+ = 0B;" 1 "original" } }
! { dg-final { scan-tree-dump-times "fgsl_file.\[0-9\]+.gsl_func = c_funptr.\[0-9\]+;" 1 "original" } }
! { dg-final { scan-tree-dump-times "c_ptr.\[0-9\]+ = 0B;" 1 "original" } }
! { dg-final { scan-tree-dump-times "fgsl_file.\[0-9\]+.gsl_file = c_ptr.\[0-9\]+;" 1 "original" } }

! { dg-final { scan-tree-dump-times "NIptr = 0B"    0 "original" } }
! { dg-final { scan-tree-dump-times "NIfunptr = 0B" 0 "original" } }

! { dg-final { scan-tree-dump-times "bbb =" 0 "original" } }

