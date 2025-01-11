! { dg-additional-options "-fdump-tree-original"  }

! The following definitions are in omp_lib, which cannot be included
! in gcc/testsuite/

module m
  use iso_c_binding
  implicit none (type, external)

  integer, parameter :: omp_interop_kind = c_intptr_t
  integer, parameter :: omp_interop_none = 0_omp_interop_kind

  interface
    real function repl1(); end  ! { dg-note "'declare variant' candidate 'repl1' declared here" }

    real function base1()
! { dg-note "'base1' declared here" "" { target *-*-* } .-1 }
      !$omp declare variant(repl1) match(construct={dispatch})
    end

    subroutine repl2 (x1, x2)  ! { dg-note "'declare variant' candidate 'repl2' declared here" }
      import
      type(c_ptr), value :: x1, x2
    end
    subroutine base2 (x, y)
! { dg-note "'base2' declared here" "" { target *-*-* } .-1 }
      import
      type(c_ptr), value :: x, y
      !$omp declare variant(repl2) match(construct={dispatch}) adjust_args(need_device_ptr : y)
    end
  end interface

contains

real function dupl (a, b)
  type(c_ptr), value :: a, b
  integer(omp_interop_kind) :: obj1, obj2
  real :: x

  !$omp dispatch interop ( obj1, obj2) device(2)
    x = base1 ()
  ! { dg-error "number of list items in 'interop' clause \\(2\\) exceeds the number of 'append_args' items \\(0\\) for 'declare variant' candidate 'repl1'" "" { target *-*-* } .-1 }

  !$omp dispatch device(9) interop ( obj1, obj2) nocontext(.true.)
    call base2 (a, b)
  ! { dg-error "unexpected 'interop' clause as invoked procedure 'base2' is not variant substituted" "" { target *-*-* } .-1 }
  dupl = x
end

real function test (a, b)
  type(c_ptr), value :: a, b
  integer(omp_interop_kind) :: obj1, obj2
  real :: x, y

  !$omp dispatch interop ( obj1 )
    x = base1 ()
  ! { dg-error "number of list items in 'interop' clause \\(1\\) exceeds the number of 'append_args' items \\(0\\) for 'declare variant' candidate 'repl1'" "" { target *-*-* } .-1 }

  !$omp dispatch interop ( obj1, obj1 ) device(42) ! Twice the same - should be fine.
    x = base1 ()
  ! { dg-error "number of list items in 'interop' clause \\(2\\) exceeds the number of 'append_args' items \\(0\\) for 'declare variant' candidate 'repl1'" "" { target *-*-* } .-1 }

  !$omp dispatch novariants(.true.) interop(obj2, obj1) device(0)
    y = base1 ()
  ! { dg-error "unexpected 'interop' clause as invoked procedure 'base1' is not variant substituted" "" { target *-*-* } .-1 }

  !$omp dispatch interop(obj2, obj1) device(3)
    call base2 (a, b)
  ! { dg-error "number of list items in 'interop' clause \\(2\\) exceeds the number of 'append_args' items \\(0\\) for 'declare variant' candidate 'repl2'" "" { target *-*-* } .-1 }

  !$omp dispatch interop(obj2) nocontext(.true.)
    call base2 (a, b)
  ! { dg-error "unexpected 'interop' clause as invoked procedure 'base2' is not variant substituted" "" { target *-*-* } .-1 }
  test = x + y
end
end module


! { dg-final { scan-tree-dump-times "#pragma omp dispatch interop\\(obj2\\) interop\\(obj1\\) device\\(2\\)\[\\n\\r\]" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp dispatch interop\\(obj2\\) interop\\(obj1\\) nocontext\\(1\\) device\\(9\\)\[\\n\\r\]" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp dispatch interop\\(obj1\\)\[\\n\\r\]" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp dispatch interop\\(obj1\\) interop\\(obj1\\) device\\(42\\)\[\\n\\r\]" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp dispatch interop\\(obj1\\) interop\\(obj2\\) novariants\\(1\\) device\\(0\\)\[\\n\\r\]" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp dispatch interop\\(obj1\\) interop\\(obj2\\) device\\(3\\)\[\\n\\r\]" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp dispatch interop\\(obj2\\) nocontext\\(1\\)\[\\n\\r\]" 1 "original" } }
