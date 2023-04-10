! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
!
! PR fortran/108621
!
! If the bind(C) procedure's dummy argument is a POINTER with INTENT(OUT),
! avoid converting the array bounds for the CFI descriptor before the call.
!
! Rational: Fewer code and, esp. for undefined pointers, there might be a
! compile-time warning or a runtime error due to the 'extent' arithmentic
! and integer overflows (i.e. random values and -fsanitize=undefined).
!
! (For disassociated pointers, it would/should be only pointless code as
! the bound setting is guarded by a != NULL condtion. However, as the PR shows,
! a bogus may-use-uninitialized-memory warning might still be shown in that case.)
!
! Without 'intent' (but still intent(out) internally), the same applies but
! there is nothing the compiler can do on the caller side.
! Still, as only uninit memory and not invalid memory it accessed, it should still
! work (at least when run-time checking is turned off).
!
subroutine demo(f)
use, intrinsic :: iso_c_binding, only : c_int
implicit none

interface
  subroutine fun(f_p) bind(c)
    import c_int
    integer(c_int), pointer, intent(out) :: f_p(:)
  end subroutine
end interface

integer(c_int), pointer :: f(:)

call fun(f)
end

! The following ones must be present even with intent(out):
!
! { dg-final { scan-tree-dump "cfi...version = 1;" "original" } }
! { dg-final { scan-tree-dump "cfi...rank = 1;" "original" } }
! { dg-final { scan-tree-dump "cfi...type = 1025;" "original" } }
! { dg-final { scan-tree-dump "cfi...attribute = 0;" "original" } }
! { dg-final { scan-tree-dump "cfi...elem_len = 4;" "original" } }


! The following is not needed - but user code might expect that an incoming pointer is NULL
! in this case. - At least the GCC testsuite expects this in the C code at
!   gfortran.dg/c-interop/section-{1,2}.f90 
! Thus, it is kept as it does not cause any harm:
!
! { dg-final { scan-tree-dump "cfi...base_addr = f->data;" "original" } }


! The following ones are not need with intent(out) and, therefore, shouldn't be there:
!
!     cfi.0.dim[idx.1].lower_bound = f->dim[idx.1].lbound;
!     cfi.0.dim[idx.1].extent = (f->dim[idx.1].ubound - f->dim[idx.1].lbound) + 1;
!     cfi.0.dim[idx.1].sm = f->dim[idx.1].stride * f->span;
!
! Now match those - but using a rather generic pattern as it is a ...-not scan:
!
! { dg-final { scan-tree-dump-not "lower_bound = " "original" } }
! { dg-final { scan-tree-dump-not "extent = " "original" } }
! { dg-final { scan-tree-dump-not "sm = " "original" } }
