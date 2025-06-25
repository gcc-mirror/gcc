! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
!
! Check the generation of NON_LVALUE_EXPR expressions in cases where a unary
! operator expression would simplify to a bare data reference.

! A NON_LVALUE_EXPR is generated for a double negation that would simplify to
! a bare data reference.
function f1 (f1_arg1)
  integer, value :: f1_arg1
  integer :: f1
  f1 = -(-f1_arg1)
end function
! { dg-final { scan-tree-dump "__result_f1 = NON_LVALUE_EXPR <f1_arg1>;" "original" } }

! A NON_LVALUE_EXPR is generated for a double complement that would simplify to
! a bare data reference.
function f2 (f2_arg1)
  integer, value :: f2_arg1
  integer :: f2
  f2 = not(not(f2_arg1))
end function
! { dg-final { scan-tree-dump "__result_f2 = NON_LVALUE_EXPR <f2_arg1>;" "original" } }

! A NON_LVALUE_EXPR is generated for a double complex conjugate that would
! simplify to a bare data reference.
function f3 (f3_arg1)
  complex, value :: f3_arg1
  complex :: f3
  f3 = conjg(conjg(f3_arg1))
end function
! { dg-final { scan-tree-dump "__result_f3 = NON_LVALUE_EXPR <f3_arg1>;" "original" } }
