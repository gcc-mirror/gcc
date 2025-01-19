! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

! Check that __builtin_omp_get_default_device and __builtin_omp_get_mapped_ptr
! are called with the right arguments depending on is_device_ptr. By default,
! Fortran passes arguments by reference, so it is important to check that:
! (1) __builtin_omp_get_mapped_ptr arguments are the actual pointers; and
! (2) f1 arguments are references to pointers.

module main
  use iso_c_binding, only: c_ptr
  implicit none
  interface
  subroutine f1 (p, p2)
    import :: c_ptr
    type(c_ptr), intent(out) :: p
    type(c_ptr), intent(in) :: p2
  end subroutine
  subroutine f2 (p, p2)
    import :: c_ptr
    type(c_ptr), intent(out) :: p
    type(c_ptr), intent(in) :: p2
  !$omp declare variant (f1) match (construct={dispatch}) adjust_args (need_device_ptr: p, p2)
  end subroutine
  end interface
  contains
  
  subroutine test ()
    type(c_ptr) :: p, p2

  ! Note there are multiple matches because every variable capturing matches in addition,
  ! i.e. scan-tree-dump-times = 1 plus number of captures used for backward references.
  !
  ! For the first scan-tree-dump, on some targets the __builtin_omp_get_mapped_ptr get
  ! swapped.

  !$omp dispatch
    ! { dg-final { scan-tree-dump-times "#pragma omp dispatch.*(D\.\[0-9]+) = __builtin_omp_get_default_device \\(\\);\[ \t\n\r]*(p2?\.\[0-9]) = p2?;\[ \t\n\r]*(D\.\[0-9]+) = __builtin_omp_get_mapped_ptr \\(\\2, \\1\\);\[ \t\n\r]*(D\.\[0-9]+) = \\3;\[ \t\n\r]*(p2?\.\[0-9]) = p2?;\[ \t\n\r]*(D\.\[0-9]+) = __builtin_omp_get_mapped_ptr \\(\\5, \\1\\);\[ \t\n\r]*(D\.\[0-9]+) = \\6;\[ \t\n\r]*f1 \\((?:&\\7, &\\4|&\\4, &\\7)\\);" 8 "gimple" } }
    call f2 (p, p2)
  !$omp dispatch is_device_ptr(p)
    ! { dg-final { scan-tree-dump-times "#pragma omp dispatch is_device_ptr\\(p\\).*(D\.\[0-9]+) = __builtin_omp_get_default_device \\(\\);\[ \t\n\r]*(p2\.\[0-9]) = p2;\[ \t\n\r]*(D\.\[0-9]+) = __builtin_omp_get_mapped_ptr \\(\\2, \\1\\);\[ \t\n\r]*(D\.\[0-9]+) = \\3;\[ \t\n\r]*f1 \\(&p, &\\4\\);" 5 "gimple" } }
    call f2 (p, p2)
  !$omp dispatch is_device_ptr(p2)
    ! { dg-final { scan-tree-dump-times "#pragma omp dispatch is_device_ptr\\(p2\\).*(D\.\[0-9]+) = __builtin_omp_get_default_device \\(\\);\[ \t\n\r]*(p\.\[0-9]) = p;\[ \t\n\r]*(D\.\[0-9]+) = __builtin_omp_get_mapped_ptr \\(\\2, \\1\\);\[ \t\n\r]*(D\.\[0-9]+) = \\3;\[ \t\n\r]*f1 \\(&\\4, &p2\\);" 5 "gimple" } }
    call f2 (p, p2)
  !$omp dispatch is_device_ptr(p, p2)
    ! { dg-final { scan-tree-dump-times "#pragma omp dispatch is_device_ptr\\(p\\) is_device_ptr\\(p2\\)\[ \t\n\r\{]*p = {CLOBBER};\[ \t\n\r]*f1 \\(&p, &p2\\);" 1 "gimple" } }
    call f2 (p, p2)
  end subroutine
end module

