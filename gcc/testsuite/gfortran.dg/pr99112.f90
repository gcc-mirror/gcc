! { dg-do compile }
! { dg-options "-fcheck=pointer -fdump-tree-original" }
! PR99112 - ICE with runtime diagnostics for SIZE intrinsic function
 
module m
  type t
  end type
contains
  function f (x, y) result(z)
    class(t) :: x(:)
    class(t) :: y(size(x))
    type(t)  :: z(size(x))
  end
  function g (x) result(z)
    class(*) :: x(:)
    type(t)  :: z(size(x))
  end
  subroutine s ()
    class(t), allocatable :: a(:), b(:), c(:), d(:)
    class(t), pointer     :: p(:)
    c = f (a, b)
    d = g (p)
  end
end
! { dg-final { scan-tree-dump-times "_gfortran_runtime_error_at" 3 "original" } }
! { dg-final { scan-tree-dump-times "Allocatable actual argument" 2 "original" } }
! { dg-final { scan-tree-dump-times "Pointer actual argument" 1 "original" } }
