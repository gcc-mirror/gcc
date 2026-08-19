! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
!
! PR fortran/126950
!
! A TARGET assumed-shape dummy is addressed through the span of its
! descriptor.  The descriptor is not mapped to the device, so the span has
! to be loaded into a local variable on entry and that variable used inside
! the target region, rather than the region dereferencing the descriptor.

module m
  use iso_c_binding
contains
  subroutine tgt (t)
    real(c_double), target :: t(:)
    !$omp target has_device_addr(t)
    call inner (t(1))
    !$omp end target
  end subroutine tgt

  subroutine inner (a)
    real(c_double) :: a
  end subroutine inner
end module m

! The span is loaded from the descriptor once, on entry.
! { dg-final { scan-tree-dump-times "span\.\[0-9\]+ = t->span;" 1 "original" } }
! The element reference uses that variable, not the descriptor.
! { dg-final { scan-tree-dump "t\.\[0-9\]+ \\+ \\(sizetype\\) \\(\\(offset\.\[0-9\]+ \\+ \[^)\]*stride\.\[0-9\]+\[^)\]*\\) \\* span\.\[0-9\]+\\)" "original" } }
! { dg-final { scan-tree-dump-not "\\* t->span" "original" } }
