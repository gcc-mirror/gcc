! { dg-do run }
!
! PR fortran/112371
! The library used to not set the bounds and content of the resulting array
! of a reduction function if the input array had zero extent along the
! reduction dimension.

program p
  implicit none
  call check_iall
  call check_iany
  call check_iparity
  call check_minloc_int
  call check_minloc_char
  call check_maxloc_real
  call check_maxloc_char
  call check_minval_int
  call check_minval_char
  call check_maxval_real
  call check_maxval_char
  call check_sum
  call check_product
contains
  subroutine check_iall
    integer :: a(3,0,2)
    logical(kind=1) :: m(3,0,2)
    integer :: i
    integer, allocatable :: r(:,:)
    a = reshape((/ integer:: /), shape(a))
    m = reshape((/ logical(kind=1):: /), shape(m))
    i = 2
    r = iall(a, dim=i, mask=m)
    if (any(lbound(r) /= 1)) stop 11
    if (any(ubound(r) /= (/ 3, 2 /))) stop 12
    if (any(shape(r) /= (/ 3, 2 /))) stop 13
    if (any(r /= int(z'FFFFFFFF'))) stop 14
  end subroutine
  subroutine check_iany
    integer(kind=8) :: a(2,3,0)
    logical(kind=1) :: m(2,3,0)
    integer :: i
    integer(kind=8), allocatable :: r(:,:)
    a = reshape((/ integer(kind=8):: /), shape(a))
    m = reshape((/ logical(kind=1):: /), shape(m))
    i = 3
    r = iany(a, dim=i, mask=m)
    if (any(lbound(r) /= 1)) stop 21
    if (any(ubound(r) /= (/ 2, 3 /))) stop 22
    if (any(shape(r) /= (/ 2, 3 /))) stop 23
    if (any(r /= 0)) stop 24
  end subroutine
  subroutine check_iparity
    integer(kind=2) :: a(0,2,3)
    logical(kind=1) :: m(0,2,3)
    integer :: i
    integer, allocatable :: r(:,:)
    a = reshape((/ integer(kind=2):: /), shape(a))
    m = reshape((/ logical(kind=1):: /), shape(m))
    i = 1
    r = iparity(a, dim=i, mask=m)
    if (any(lbound(r) /= 1)) stop 31
    if (any(ubound(r) /= (/ 2, 3 /))) stop 32
    if (any(shape(r) /= (/ 2, 3 /))) stop 33
    if (any(r /= 0)) stop 34
  end subroutine
  subroutine check_minloc_int
    integer :: a(3,0,2)
    logical(kind=1) :: m(3,0,2)
    integer :: i, j
    integer, allocatable :: r(:,:)
    a = reshape((/ integer:: /), shape(a))
    m = reshape((/ logical(kind=1):: /), shape(m))
    i = 2
    r = minloc(a, dim=i, mask=m)
    if (any(lbound(r) /= 1)) stop 41
    if (any(ubound(r) /= (/ 3, 2 /))) stop 42
    if (any(shape(r) /= (/ 3, 2 /))) stop 43
    if (any(r /= 0)) stop 44
  end subroutine
  subroutine check_minloc_char
    character :: a(2,3,0)
    logical(kind=1) :: m(2,3,0)
    integer :: i
    integer, allocatable :: r(:,:)
    a = reshape((/ character:: /), shape(a))
    m = reshape((/ logical(kind=1):: /), shape(m))
    i = 3
    r = minloc(a, dim=i, mask=m)
    if (any(lbound(r) /= 1)) stop 51
    if (any(ubound(r) /= (/ 2, 3 /))) stop 52
    if (any(shape(r) /= (/ 2, 3 /))) stop 53
    if (any(r /= 0)) stop 54
  end subroutine
  subroutine check_maxloc_real
    real :: a(0,2,3)
    logical(kind=1) :: m(0,2,3)
    integer :: i
    integer, allocatable :: r(:,:)
    a = reshape((/ real:: /), shape(a))
    m = reshape((/ logical(kind=1):: /), shape(m))
    i = 1
    r = maxloc(a, dim=i, mask=m)
    if (any(lbound(r) /= 1)) stop 61
    if (any(ubound(r) /= (/ 2, 3 /))) stop 62
    if (any(shape(r) /= (/ 2, 3 /))) stop 63
    if (any(r /= 0)) stop 64
  end subroutine
  subroutine check_maxloc_char
    character(len=2) :: a(3,0,2)
    logical(kind=1) :: m(3,0,2)
    integer :: i
    integer, allocatable :: r(:,:)
    a = reshape((/ character(len=2):: /), shape(a))
    m = reshape((/ logical(kind=1):: /), shape(m))
    i = 2
    r = maxloc(a, dim=i, mask=m)
    if (any(lbound(r) /= 1)) stop 71
    if (any(ubound(r) /= (/ 3, 2 /))) stop 72
    if (any(shape(r) /= (/ 3, 2 /))) stop 73
    if (any(r /= 0)) stop 74
  end subroutine
  subroutine check_minval_int
    integer(kind=2) :: a(3,2,0)
    logical(kind=1) :: m(3,2,0)
    integer :: i, j
    integer, allocatable :: r(:,:)
    a = reshape((/ integer(kind=2):: /), shape(a))
    m = reshape((/ logical(kind=1):: /), shape(m))
    i = 3
    r = minval(a, dim=i, mask=m)
    if (any(lbound(r) /= 1)) stop 81
    if (any(ubound(r) /= (/ 3, 2 /))) stop 82
    if (any(shape(r) /= (/ 3, 2 /))) stop 83
    if (any(r /= huge(1_2))) stop 84
  end subroutine
  subroutine check_minval_char
    character(kind=4) :: a(0,3,2)
    logical(kind=1) :: m(0,3,2)
    integer :: i
    character(kind=4), allocatable :: r(:,:)
    a = reshape((/ character(kind=4):: /), shape(a))
    m = reshape((/ logical(kind=1):: /), shape(m))
    i = 1
    r = minval(a, dim=i, mask=m)
    if (any(lbound(r) /= 1)) stop 91
    if (any(ubound(r) /= (/ 3, 2 /))) stop 92
    if (any(shape(r) /= (/ 3, 2 /))) stop 93
    if (any(r /= char(int(z'FFFFFFFF', kind=8), kind=4))) stop 94
  end subroutine
  subroutine check_maxval_real
    real(kind=8) :: a(0,2,3)
    logical(kind=1) :: m(0,2,3)
    integer :: i
    real(kind=8), allocatable :: r(:,:)
    a = reshape((/ real(kind=8):: /), shape(a))
    m = reshape((/ logical(kind=1):: /), shape(m))
    i = 1
    r = maxval(a, dim=i, mask=m)
    if (any(lbound(r) /= 1)) stop 101
    if (any(ubound(r) /= (/ 2, 3 /))) stop 102
    if (any(shape(r) /= (/ 2, 3 /))) stop 103
    if (any(r /= -huge(1._8))) stop 104
  end subroutine
  subroutine check_maxval_char
    character(kind=4,len=2) :: a(3,0,2), e
    logical(kind=1) :: m(3,0,2)
    integer :: i
    character(len=2,kind=4), allocatable :: r(:,:)
    a = reshape((/ character(kind=4,len=2):: /), shape(a))
    m = reshape((/ logical(kind=1):: /), shape(m))
    i = 2
    r = maxval(a, dim=i, mask=m)
    if (any(lbound(r) /= 1)) stop 111
    if (any(ubound(r) /= (/ 3, 2 /))) stop 112
    if (any(shape(r) /= (/ 3, 2 /))) stop 113
    e = repeat(char(0, kind=4), len(a))
    if (any(r /= e)) stop 114
  end subroutine
  subroutine check_sum
    integer(kind=1) :: a(2,3,0)
    logical(kind=1) :: m(2,3,0)
    integer :: i
    integer, allocatable :: r(:,:)
    a = reshape((/ integer:: /), shape(a))
    m = reshape((/ logical(kind=1):: /), shape(m))
    i = 3
    r = sum(a, dim=i, mask=m)
    if (any(lbound(r) /= 1)) stop 121
    if (any(ubound(r) /= (/ 2, 3 /))) stop 122
    if (any(shape(r) /= (/ 2, 3 /))) stop 123
    if (any(r /= 0)) stop 124
  end subroutine
  subroutine check_product
    real(kind=8) :: a(0,2,3)
    logical(kind=1) :: m(0,2,3)
    integer :: i
    integer, allocatable :: r(:,:)
    a = reshape((/ real(kind=8):: /), shape(a))
    m = reshape((/ logical(kind=1):: /), shape(m))
    i = 1
    r = product(a, dim=i, mask=m)
    if (any(lbound(r) /= 1)) stop 131
    if (any(ubound(r) /= (/ 2, 3 /))) stop 132
    if (any(shape(r) /= (/ 2, 3 /))) stop 133
    if (any(r /= 1.0_8)) stop 134
  end subroutine
end program
