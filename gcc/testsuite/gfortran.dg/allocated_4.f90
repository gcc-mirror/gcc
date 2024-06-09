! { dg-do run }
!
! PR fortran/112412
! The library used to not allocate memory for the result of transformational
! functions reducing an array along one dimension, if the result of the
! function was an empty array.  This caused the result to be seen as
! an unallocated array.

program p
  implicit none
  call check_iparity
  call check_sum
  call check_minloc_int
  call check_minloc_char
  call check_maxloc_char4
  call check_minval_char
  call check_maxval_char4
  call check_any
  call check_count4
contains
  subroutine check_iparity
    integer :: a(9,3,0,7)
    logical :: m1(9,3,0,7)
    logical(kind=4) :: m4
    integer :: i
    integer, allocatable :: r(:,:,:)
    a  = reshape((/ integer:: /), shape(a))
    m1 = reshape((/ logical:: /), shape(m1))
    m4 = .false.
    i = 1
    r = iparity(a, dim=i)
    if (.not. allocated(r)) stop 11
    deallocate(r)
    i = 2
    r = iparity(a, dim=i, mask=m1)
    if (.not. allocated(r)) stop 12
    deallocate(r)
    i = 4
    r = iparity(a, dim=i, mask=m4)
    if (.not. allocated(r)) stop 13
    deallocate(r)
  end subroutine
  subroutine check_sum
    integer :: a(9,3,0,7)
    logical :: m1(9,3,0,7)
    logical(kind=4) :: m4
    integer :: i
    integer, allocatable :: r(:,:,:)
    a  = reshape((/ integer:: /), shape(a))
    m1 = reshape((/ logical:: /), shape(m1))
    m4 = .false.
    i = 2
    r = sum(a, dim=i)
    if (.not. allocated(r)) stop 21
    deallocate(r)
    i = 4
    r = sum(a, dim=i, mask=m1)
    if (.not. allocated(r)) stop 22
    deallocate(r)
    i = 1
    r = sum(a, dim=i, mask=m4)
    if (.not. allocated(r)) stop 23
    deallocate(r)
  end subroutine
  subroutine check_minloc_int
    integer :: a(9,3,0,7)
    logical :: m1(9,3,0,7)
    logical(kind=4) :: m4
    integer :: i
    integer, allocatable :: r(:,:,:)
    a  = reshape((/ integer:: /), shape(a))
    m1 = reshape((/ logical:: /), shape(m1))
    m4 = .false.
    i = 4
    r = minloc(a, dim=i)
    if (.not. allocated(r)) stop 31
    deallocate(r)
    i = 1
    r = minloc(a, dim=i, mask=m1)
    if (.not. allocated(r)) stop 32
    deallocate(r)
    i = 2
    r = minloc(a, dim=i, mask=m4)
    if (.not. allocated(r)) stop 33
    deallocate(r)
  end subroutine
  subroutine check_minloc_char
    character :: a(9,3,0,7)
    logical :: m1(9,3,0,7)
    logical(kind=4) :: m4
    integer :: i
    integer, allocatable :: r(:,:,:)
    a  = reshape((/ character:: /), shape(a))
    m1 = reshape((/ logical:: /), shape(m1))
    m4 = .false.
    i = 4
    r = minloc(a, dim=i)
    if (.not. allocated(r)) stop 41
    deallocate(r)
    i = 2
    r = minloc(a, dim=i, mask=m1)
    if (.not. allocated(r)) stop 42
    deallocate(r)
    i = 1
    r = minloc(a, dim=i, mask=m4)
    if (.not. allocated(r)) stop 43
    deallocate(r)
  end subroutine
  subroutine check_maxloc_char4
    character(kind=4) :: a(9,3,0,7)
    logical :: m1(9,3,0,7)
    logical(kind=4) :: m4
    integer :: i
    integer, allocatable :: r(:,:,:)
    a  = reshape((/ character(kind=4):: /), shape(a))
    m1 = reshape((/ logical:: /), shape(m1))
    m4 = .false.
    i = 1
    r = maxloc(a, dim=i)
    if (.not. allocated(r)) stop 51
    deallocate(r)
    i = 4
    r = maxloc(a, dim=i, mask=m1)
    if (.not. allocated(r)) stop 52
    deallocate(r)
    i = 2
    r = maxloc(a, dim=i, mask=m4)
    if (.not. allocated(r)) stop 53
    deallocate(r)
  end subroutine
  subroutine check_minval_char
    character :: a(9,3,0,7)
    logical :: m1(9,3,0,7)
    logical(kind=4) :: m4
    integer :: i
    character, allocatable :: r(:,:,:)
    a  = reshape((/ character:: /), shape(a))
    m1 = reshape((/ logical:: /), shape(m1))
    m4 = .false.
    i = 2
    r = minval(a, dim=i)
    if (.not. allocated(r)) stop 61
    deallocate(r)
    i = 1
    r = minval(a, dim=i, mask=m1)
    if (.not. allocated(r)) stop 62
    deallocate(r)
    i = 4
    r = minval(a, dim=i, mask=m4)
    if (.not. allocated(r)) stop 63
    deallocate(r)
  end subroutine
  subroutine check_maxval_char4
    character(kind=4) :: a(9,3,0,7)
    logical :: m1(9,3,0,7)
    logical(kind=4) :: m4
    integer :: i
    character(kind=4), allocatable :: r(:,:,:)
    a  = reshape((/ character(kind=4):: /), shape(a))
    m1 = reshape((/ logical:: /), shape(m1))
    m4 = .false.
    i = 1
    r = maxval(a, dim=i)
    if (.not. allocated(r)) stop 71
    deallocate(r)
    i = 2
    r = maxval(a, dim=i, mask=m1)
    if (.not. allocated(r)) stop 72
    deallocate(r)
    i = 4
    r = maxval(a, dim=i, mask=m4)
    if (.not. allocated(r)) stop 73
    deallocate(r)
  end subroutine
  subroutine check_any
    logical :: a(9,3,0,7)
    integer :: i
    logical, allocatable :: r(:,:,:)
    a  = reshape((/ logical:: /), shape(a))
    i = 2
    r = any(a, dim=i)
    if (.not. allocated(r)) stop 81
    deallocate(r)
  end subroutine
  subroutine check_count4
    logical(kind=4) :: a(9,3,0,7)
    integer :: i
    integer, allocatable :: r(:,:,:)
    a  = reshape((/ logical(kind=4):: /), shape(a))
    i = 4
    r = count(a, dim=i)
    if (.not. allocated(r)) stop 91
    deallocate(r)
  end subroutine
end program
