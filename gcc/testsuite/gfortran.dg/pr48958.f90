! { dg-do run }
! { dg-options "-fcheck=pointer -fdump-tree-original" }
! { dg-shouldfail "Fortran runtime error: Allocatable argument 'a' is not allocated" }
! { dg-output "At line 13 .*" }
! PR48958 - Add runtime diagnostics for SIZE intrinsic function

program p
  integer :: n
  integer,  allocatable :: a(:)
  integer,  pointer     :: b(:)
  class(*), allocatable :: c(:)
  integer               :: d(10)
  print *, size (a)
  print *, size (b)
  print *, size (c)
  print *, size (d)
  print *, size (f(n))
contains
  function f (n)
    integer, intent(in) :: n
    real, allocatable   :: f(:)
  end function f
end

! { dg-final { scan-tree-dump-times "_gfortran_runtime_error_at" 4 "original" } }
