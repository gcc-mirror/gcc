! Compiled with pr120049_b.f90
! { dg-options -O0 }
! { dg-do compile }
! { dg-compile-aux-modules "pr120049_b.f90" }
!
! Test the fix for PR120049
program tests_gtk_sup
  use gtk_sup
  implicit none
  
  type mytype
    integer :: myint
  end type mytype
  type(mytype) :: ijkl = mytype(42)
  logical :: truth
  real :: var1
  type(c_ptr), target :: val
  type(c_funptr), target :: fptr
  character(15) :: stringy
  complex :: certainly
  truth = .true.
  var1 = 86.
  stringy = "what the hay!"
  certainly = (3.14,-4.13)
  if (c_associated(val, c_loc(val))) then
    stop 1
  endif
  if (c_associated(c_loc(val), val)) then
    stop 2
  endif
  print *, c_associated(fptr, C_NULL_FUNPTR)
  print *, c_associated(c_loc(val), C_NULL_PTR)
  print *, c_associated(C_NULL_PTR, c_loc(val)) 
  print *, c_associated(c_loc(val), 42) ! { dg-error "C_ASSOCIATED shall have the" }
  print *, c_associated(c_loc(val), .42) ! { dg-error "C_ASSOCIATED shall have the" }
  print *, c_associated(c_loc(val), truth) ! { dg-error "C_ASSOCIATED shall have the" }
  print *, c_associated(c_loc(val), .false.) ! { dg-error "C_ASSOCIATED shall have the" }
  print *, c_associated(c_loc(val), var1) ! { dg-error "C_ASSOCIATED shall have the" }
  print *, c_associated(c_loc(val), stringy) ! { dg-error "C_ASSOCIATED shall have the" }
  print *, c_associated(c_loc(val), certainly) ! { dg-error "C_ASSOCIATED shall have the" }
  print *, c_associated(42) ! { dg-error "C_ASSOCIATED shall have the" }
  print *, c_associated(.42) ! { dg-error "C_ASSOCIATED shall have the" }
  print *, c_associated(truth) ! { dg-error "C_ASSOCIATED shall have the" }
  print *, c_associated(.false.) ! { dg-error "C_ASSOCIATED shall have the" }
  print *, c_associated(var1) ! { dg-error "C_ASSOCIATED shall have the" }
  print *, c_associated(stringy) ! { dg-error "C_ASSOCIATED shall have the" }
  print *, c_associated(certainly) ! { dg-error "C_ASSOCIATED shall have the" }
  print *, c_associated(.42) ! { dg-error "C_ASSOCIATED shall have the" }
  print *, c_associated(val, testit(val)) ! { dg-error "C_ASSOCIATED shall have the" }
  print *, c_associated(testit(val), val) ! { dg-error "C_ASSOCIATED shall have the" }
  print *, c_associated(testit(val)) ! { dg-error "C_ASSOCIATED shall have the" }
  print *, c_associated(c_loc(val), C_NULL_FUNPTR) ! { dg-error "C_ASSOCIATED shall have the" }
  print *, c_associated(C_NULL_FUNPTR, c_loc(val)) ! { dg-error "C_ASSOCIATED shall have the" }
contains

  function testit (avalue) result(res)
    type(c_ptr) :: avalue
    type(mytype) :: res
    res%myint = 42
  end function

end program tests_gtk_sup
