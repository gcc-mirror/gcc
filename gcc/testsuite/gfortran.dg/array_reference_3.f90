! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
!
! PR fortran/102043
! Array indexing was causing the middle-end to conclude the index
! to be non-negative, which can be wrong for arrays with a "reversed-order"
! descriptor.  This was fixed by using pointer arithmetic when
! the index can be negative.
! 
! This test checks the code generated for array references of various kinds
! of arrays, using either array indexing or pointer arithmetic.

program p
  implicit none
  call check_assumed_shape_elem
  call check_assumed_shape_scalarized
  call check_descriptor_dim
  call check_cfi_dim
  call check_substring
  call check_ptr_elem
  call check_ptr_scalarized
  call check_explicit_shape_elem
  call check_explicit_shape_scalarized
  call check_tmp_array
  call check_allocatable_array_elem
  call check_allocatable_array_scalarized
contains
  subroutine cases(assumed_shape_x)
    integer :: assumed_shape_x(:)
    assumed_shape_x(2) = 10
  end subroutine cases 
  subroutine check_assumed_shape_elem
    integer :: x(3)
    x = 0
    call cases(x)
    if (any(x /= (/ 0, 10, 0 /))) stop 10
    ! Assumed shape array are referenced with pointer arithmetic.
    ! { dg-final { scan-tree-dump-times "\\*\\(\\(integer\\(kind=4\\) \\*\\) assumed_shape_x.\\d+ \\+ \\(sizetype\\) \\(\\(stride.\\d+ \\* 2 \\+ offset.\\d+\\) \\* 4\\)\\) = 10;" 1 "original" } }
  end subroutine check_assumed_shape_elem
  subroutine casss(assumed_shape_y)
    integer :: assumed_shape_y(:)
    assumed_shape_y = 11
  end subroutine casss 
  subroutine check_assumed_shape_scalarized
    integer :: y(3)
    call casss(y)
    if (any(y /= 11)) stop 11
    ! Assumed shape array are referenced with pointer arithmetic.
    ! { dg-final { scan-tree-dump-times "\\*\\(\\(integer\\(kind=4\\) \\*\\) assumed_shape_y.\\d+ \\+ \\(sizetype\\) \\(\\(S.\\d+ \\* D.\\d+ \\+ D.\\d+\\) \\* 4\\)\\) = 11;" 1 "original" } }
  end subroutine check_assumed_shape_scalarized
  subroutine check_descriptor_dim
    integer, allocatable :: descriptor(:)
    allocate(descriptor(4))
    descriptor(:) = 12
    if (any(descriptor /= 12)) stop 12
    ! The descriptor’s dim array is referenced with array indexing.
    ! { dg-final { scan-tree-dump-times "descriptor\\.dim\\\[0\\\]\\.ubound = 4;" 1 "original" } }
  end subroutine check_descriptor_dim
  subroutine ccfis(cfi_descriptor) bind(c)
    integer :: cfi_descriptor(:)
    cfi_descriptor = 13
  end subroutine ccfis 
  subroutine check_cfi_dim 
    integer :: x(5)
    call ccfis(x)
    if (any(x /= 13)) stop 13
    ! The cfi descriptor’s dim array is referenced with array indexing.
    ! { dg-final { scan-tree-dump-times "cfi_descriptor->dim\\\[idx.\\d+\\\]\\.ubound = _cfi_descriptor->dim\\\[idx.\\d+\\\]\\.extent \\+ \\(cfi_descriptor->dim\\\[idx.\\d+\\\]\\.lbound \\+ -1\\);" 1 "original" } }
  end subroutine check_cfi_dim
  subroutine css(c) bind(c)
    character :: c
    c = 'k'
  end subroutine css
  subroutine check_substring
    character(5) :: x
    x = 'abcde'
    call css(x(3:3))
    if (x /= 'abkde') stop 14
    ! Substrings use array indexing
    ! { dg-final { scan-tree-dump-times "css \\(\\(character\\(kind=1\\)\\\[\\d+:\\d+\\\] \\*\\) &x\\\[3\\\].lb: \\d+ sz: \\d+.\\);" 1 "original" } }
  end subroutine check_substring
  subroutine check_ptr_elem
    integer, target :: x(7)
    integer, pointer :: ptr_x(:)
    x = 0
    ptr_x => x
    ptr_x(4) = 16
    if (any(ptr_x /= (/ 0, 0, 0, 16, 0, 0, 0 /))) stop 16
    ! pointers are referenced with pointer arithmetic.
    ! { dg-final { scan-tree-dump-times "\\*\\(integer\\(kind=4\\) \\*\\) \\(ptr_x\\.data \\+ \\(sizetype\\) \\(\\(ptr_x\\.offset \\+ ptr_x\\.dim\\\[0\\\]\\.stride \\* 4\\) \\* ptr_x\\.span\\)\\) = 16;" 1 "original" } }
  end subroutine check_ptr_elem
  subroutine check_ptr_scalarized
    integer, target :: y(8)
    integer, pointer :: ptr_y(:)
    y = 0
    ptr_y => y
    ptr_y = 17
    if (any(ptr_y /= 17)) stop 17
    ! pointers are referenced with pointer arithmetic.
    ! { dg-final { scan-tree-dump-times "\\*\\(\\(integer\\(kind=4\\) \\*\\) D.\\d+ \\+ \\(sizetype\\) \\(\\(S.\\d+ \\* D.\\d+ \\+ D.\\d+\\) \\* ptr_y\\.span\\)\\) = 17;" 1 "original" } }
  end subroutine check_ptr_scalarized
  subroutine check_explicit_shape_elem
    integer :: explicit_shape_x(9)
    explicit_shape_x = 0
    explicit_shape_x(5) = 18
    if (any(explicit_shape_x /= (/ 0, 0, 0, 0, 18, 0, 0, 0, 0 /))) stop 18
    ! Explicit shape arrays are referenced with array indexing.
    ! { dg-final { scan-tree-dump-times "explicit_shape_x\\\[4\\\] = 18;" 1 "original" } }
  end subroutine check_explicit_shape_elem
  subroutine check_explicit_shape_scalarized
    integer :: explicit_shape_y(3)
    explicit_shape_y = 19
    if (any(explicit_shape_y /= 19)) stop 19
    ! Explicit shape arrays are referenced with array indexing.
    ! { dg-final { scan-tree-dump-times "explicit_shape_y\\\[S.\\d+ \\+ -1\\\] = 19;" 1 "original" } }
  end subroutine check_explicit_shape_scalarized
  subroutine check_tmp_array
    integer :: non_tmp(6)
    non_tmp = 15
    non_tmp(2:5) = non_tmp(1:4) + non_tmp(3:6)
    if (any(non_tmp /= (/ 15, 30, 30, 30, 30, 15 /))) stop 15
    ! temporary arrays use array indexing
    ! { dg-final { scan-tree-dump-times "\\(*\\(integer\\(kind=4\\)\\\[4\\\] \\* restrict\\) atmp.\\d+\\.data\\)\\\[S.\\d+\\\] = non_tmp\\\[S.\\d+\\\] \\+ non_tmp\\\[S.\\d+ \\+ 2\\\];" 1 "original" } }
    ! { dg-final { scan-tree-dump-times "non_tmp\\\[S.\\d+ \\+ 1\\\] = \\(\\*\\(integer\\(kind=4\\)\\\[4\\\] \\* restrict\\) atmp.\\d+\\.data\\)\\\[S.\\d+\\\];" 1 "original" } }
  end subroutine check_tmp_array
  subroutine check_allocatable_array_elem
    integer, allocatable :: allocatable_x(:)
    allocate(allocatable_x(4),source=0)
    allocatable_x(2) = 20
    if (any(allocatable_x /= (/ 0, 20, 0, 0 /))) stop 20
    ! Allocatable arrays are referenced with array indexing.
    ! { dg-final { scan-tree-dump-times "\\(\\*\\(integer\\(kind=4\\)\\\[0:\\\] \\* restrict\\) allocatable_x\\.data\\)\\\[allocatable_x\\.offset \\+ 2\\\] = 20;" 1 "original" } }
  end subroutine check_allocatable_array_elem
  subroutine check_allocatable_array_scalarized
    integer, allocatable :: allocatable_y(:)
    allocate(allocatable_y(5),source=0)
    allocatable_y = 21
    if (any(allocatable_y /= 21)) stop 21
    ! Allocatable arrays are referenced with array indexing.
    ! { dg-final { scan-tree-dump-times "\\(\\*D.\\d+\\)\\\[S.\\d+ \\+ \\D.\\d+\\\] = 21;" 1 "original" } }
  end subroutine check_allocatable_array_scalarized
  subroutine cares(assumed_rank_x)
    integer :: assumed_rank_x(..)
    select rank(rank_1_var_x => assumed_rank_x)
      rank(1)
        rank_1_var_x(3) = 22
    end select
  end subroutine cares 
  subroutine check_assumed_rank_elem
    integer :: x(6)
    x = 0
    call cares(x)
    if (any(x /= (/ 0, 0, 22, 0, 0, 0 /))) stop 22
    ! Assumed rank arrays are referenced with pointer arithmetic.
    ! { dg-final { scan-tree-dump-times "\\*\\(\\(integer\\(kind=4\\) \\*\\) __tmp_INTEGER_4_rank_1\\.data \\+ \\(sizetype\\) \\(\\(__tmp_INTEGER_4_rank_1\\.offset \\+ __tmp_INTEGER_4_rank_1\\.dim\\\[0\\\]\\.stride \\* 3\\) \\* 4\\)\\) = 22;" 1 "original" } }
  end subroutine check_assumed_rank_elem
  subroutine carss(assumed_rank_y)
    integer :: assumed_rank_y(..)
    select rank(rank_1_var_y => assumed_rank_y)
      rank(1)
        rank_1_var_y = 23
    end select
  end subroutine carss 
  subroutine check_assumed_rank_scalarized
    integer :: y(7)
    call carss(y)
    if (any(y /= 23)) stop 23
    ! Assumed rank arrays are referenced with pointer arithmetic.
    ! { dg-final { scan-tree-dump-times "\\*\\(\\(integer\\(kind=4\\) \\*\\) D.\\d+ \\+ \\(sizetype\\) \\(\\(S.\\d+ \\* D.\\d+ \\+ D.\\d+\\) \\* 4\\)\\) = 23;" 1 "original" } }
  end subroutine check_assumed_rank_scalarized
  subroutine casces(assumed_shape_cont_x)
    integer, dimension(:), contiguous :: assumed_shape_cont_x
    assumed_shape_cont_x(4) = 24
  end subroutine casces 
  subroutine check_assumed_shape_cont_elem
    integer :: x(8)
    x = 0
    call casces(x)
    if (any(x /= (/ 0, 0, 0, 24, 0, 0, 0, 0 /))) stop 24
    ! Contiguous assumed shape arrays are referenced with array indexing.
    ! { dg-final { scan-tree-dump-times "\\(\\*assumed_shape_cont_x.\\d+\\)\\\[stride.\\d+ \\* 4 \\+ offset.\\d+\\\] = 24;" 1 "original" } }
  end subroutine check_assumed_shape_cont_elem
  subroutine cascss(assumed_shape_cont_y)
    integer, dimension(:), contiguous :: assumed_shape_cont_y
    assumed_shape_cont_y = 25
  end subroutine cascss 
  subroutine check_assumed_shape_cont_scalarized
    integer :: y(9)
    call cascss(y)
    if (any(y /= 25)) stop 25
    ! Contiguous assumed shape arrays are referenced with array indexing.
    ! { dg-final { scan-tree-dump-times "\\(\\*assumed_shape_cont_y.\\d+\\)\\\[S.\\d+ \\* D.\\d+ \\+ D.\\d+\\\] = 25;" 1 "original" } }
  end subroutine check_assumed_shape_cont_scalarized
end program p

