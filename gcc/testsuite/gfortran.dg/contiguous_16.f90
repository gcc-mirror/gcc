! { dg-do run }
! { dg-options "-O2 -fdump-tree-original" }
!
! PR fortran/122977 - associate to a contiguous pointer

program foo
  integer, dimension(:), pointer, contiguous :: a
  integer, dimension(:), allocatable         :: u
  allocate (a(4), u(4))
  if (.not. is_contiguous(a)) error stop 1      ! optimized
  if (.not. is_contiguous(u)) error stop 2      ! optimized

  associate (b => a)
    if (.not. is_contiguous(b)) error stop 3    ! optimized
    associate (c => b)
      if (.not. is_contiguous(c)) error stop 4  ! optimized
    end associate
    associate (c => b(1::2))
      if (is_contiguous(c)) stop 11             ! runtime check
    end associate
  end associate

  associate (v => u)
    if (.not. is_contiguous(v)) error stop 5    ! optimized
    associate (w => v)
      if (.not. is_contiguous(w)) error stop 6  ! optimized
    end associate
    associate (w => v(1::2))
      if (is_contiguous(w)) stop 12             ! runtime check
    end associate
  end associate

  associate (b => a(1::2))
    if (is_contiguous(b)) stop 13               ! runtime check
    associate (c => b)
      if (is_contiguous(c)) stop 14             ! runtime check
    end associate
  end associate

  associate (v => u(1::2))
    if (is_contiguous(v)) stop 15               ! runtime check
    associate (w => v)
      if (is_contiguous(w)) stop 16             ! runtime check
    end associate
  end associate

  deallocate (a, u)
end program foo

! { dg-final { scan-tree-dump-not "_gfortran_error_stop_numeric" "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_stop_numeric" 6 "original" } }
