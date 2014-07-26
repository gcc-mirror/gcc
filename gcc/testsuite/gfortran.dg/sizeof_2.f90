! { dg-do compile }
!
! PR fortran/56650
! PR fortran/36437
!
subroutine foo(x, y)
  use iso_c_binding
  type(*) :: x
  integer :: y(*)
  integer(8) :: ii
  procedure() :: proc

  ii = sizeof (x) ! { dg-error "'x' argument of 'sizeof' intrinsic at \\(1\\) shall not be TYPE\\(\\*\\)" }
  ii = c_sizeof (x) ! { dg-error "Assumed-type argument at .1. is not permitted as actual argument to the intrinsic c_sizeof" }
  ii = storage_size (x) ! { dg-error "Assumed-type argument at .1. is not permitted as actual argument to the intrinsic storage_size" }

  ii = sizeof (y) ! { dg-error "shall not be an assumed-size array" }
  ii = c_sizeof (y) ! { dg-error "shall not be an assumed-size array" }
  ii = storage_size (y) ! okay, element-size is known

  ii = sizeof (proc) ! { dg-error "shall not be a procedure" }
  ii = c_sizeof (proc) ! { dg-error "Procedure unexpected as argument" }
  ii = storage_size (proc) ! { dg-error "shall not be a procedure" }
end
