! { dg-do run }
! { dg-additional-sources assumed_rank_22_aux.c }
! { dg-additional-options "-fdump-tree-original" }
!
! FIXME: wrong extend in array descriptor, see C file.
! { dg-output "c_assumed - 40 - OK" { xfail *-*-* } }
! { dg-output "c_assumed - 100 - OK" { xfail *-*-* } }
!
! PR fortran/94070
!
! Contributed by Tobias Burnus
! and José Rui Faustino de Sousa
!
program main
  implicit none
  integer :: A(5,4,2)
  integer, allocatable :: B(:,:,:)
  integer :: C(5,4,-2:-1)

  interface
    subroutine c_assumed (x, num) bind(C)
      integer :: x(..)
      integer, value :: num
    end subroutine
    subroutine c_allocated (x) bind(C)
      integer, allocatable :: x(..)
    end subroutine
  end interface

  allocate (B(-1:3,4,-1:-1))

  call caller (a)          ! num=0: assumed-size
  call test (b, num=20)           ! full array
  call test (b(:,:,0:-1), num=40) ! zero-sized array
  call test (c, num=60)
  call test (c(:,:,:-1), num=80) ! full-size slice
  call test (c(:,:,1:-1), num=100) !zero-size array

  call test_alloc(b)

  call c_assumed (b, num=20)
  call c_assumed (b(:,:,0:-1), num=40)
  call c_assumed (c, num=60)
  call c_assumed (c(:,:,:-1), num=80)
  call c_assumed (c(:,:,1:-1), num=100)

  call c_allocated (b)
contains
  subroutine caller(y)
    integer :: y(-1:3,4,*)
    call test(y, num=0)
    call c_assumed (y, num=0)
  end
  subroutine test (x, num)
    integer :: x(..), num

    ! SIZE (x)
    if (num == 0) then
      if (size (x) /= -20) stop 1
    elseif (num == 20) then
      if (size (x) /= 20) stop 21
    elseif (num == 40) then
      if (size (x) /= 0) stop 41
    elseif (num == 60) then
      if (size (x) /= 40) stop 61
    elseif (num == 80) then
      if (size (x) /= 40) stop 81
    elseif (num == 100) then
      if (size (x) /= 0) stop 101
    else
      stop 99  ! Invalid num
    endif

    ! SIZE (x, dim=...)
    if (size (x, dim=1) /= 5) stop num + 2
    if (size (x, dim=2) /= 4) stop num + 3

    if (num == 0) then
      if (size (x, dim=3) /= -1) stop 4
    elseif (num == 20) then
      if (size (x, dim=3) /= 1) stop 24
    elseif (num == 40) then
      if (size (x, dim=3) /= 0) stop 44
    elseif (num == 60) then
      if (size (x, dim=3) /= 2) stop 64
    elseif (num == 80) then
      if (size (x, dim=3) /= 2) stop 84
    elseif (num == 100) then
      if (size (x, dim=3) /= 0) stop 104
    endif

    ! SHAPE (x)
    if (num == 0) then
      if (any (shape (x) /= [5, 4, -1])) stop 5
    elseif (num == 20) then
      if (any (shape (x) /= [5, 4, 1])) stop 25
    elseif (num == 40) then
      if (any (shape (x) /= [5, 4, 0])) stop 45
    elseif (num == 60) then
      if (any (shape (x) /= [5, 4, 2])) stop 65
    elseif (num == 80) then
      if (any (shape (x) /= [5, 4, 2])) stop 85
    elseif (num == 100) then
      if (any (shape (x) /= [5, 4, 0])) stop 105
    endif

    ! LBOUND (X)
    if (any (lbound (x) /= [1, 1, 1])) stop num + 6

    ! LBOUND (X, dim=...)
    if (lbound (x, dim=1) /= 1) stop num + 7
    if (lbound (x, dim=2) /= 1) stop num + 8
    if (lbound (x, dim=3) /= 1) stop num + 9

    ! UBOUND (X)
    if (num == 0) then
      if (any (ubound (x) /= [5, 4, -1])) stop 11
    elseif (num == 20) then
      if (any (ubound (x) /= [5, 4, 1])) stop 31
    elseif (num == 40) then
      if (any (ubound (x) /= [5, 4, 0])) stop 51
    elseif (num == 60) then
      if (any (ubound (x) /= [5, 4, 2])) stop 71
    elseif (num == 80) then
      if (any (ubound (x) /= [5, 4, 2])) stop 91
    elseif (num == 100) then
      if (any (ubound (x) /= [5, 4, 0])) stop 111
    endif

    ! UBOUND (X, dim=...)
    if (ubound (x, dim=1) /= 5) stop num + 12
    if (ubound (x, dim=2) /= 4) stop num + 13
    if (num == 0) then
      if (ubound (x, dim=3) /= -1) stop 14
    elseif (num == 20) then
      if (ubound (x, dim=3) /= 1) stop 34
    elseif (num == 40) then
      if (ubound (x, dim=3) /= 0) stop 54
    elseif (num == 60) then
      if (ubound (x, dim=3) /= 2) stop 74
    elseif (num == 80) then
      if (ubound (x, dim=3) /= 2) stop 94
    elseif (num == 100) then
      if (ubound (x, dim=3) /= 0) stop 114
    endif
  end

  subroutine test_alloc (x)
    integer, allocatable :: x(..)

    if (size (x) /= 20) stop 61
    if (size (x, dim=1) /= 5) stop 62
    if (size (x, dim=2) /= 4) stop 63
    if (size (x, dim=3) /= 1) stop 64

    if (any (shape (x) /= [5, 4, 1])) stop 65

    if (any (lbound (x) /= [-1, 1, -1])) stop 66
    if (lbound (x, dim=1) /= -1) stop 77
    if (lbound (x, dim=2) /= 1) stop 78
    if (lbound (x, dim=3) /= -1) stop 79

    if (any (ubound (x) /= [3, 4, -1])) stop 80
    if (ubound (x, dim=1) /= 3) stop 92
    if (ubound (x, dim=2) /= 4) stop 93
    if (ubound (x, dim=3) /= -1) stop 94
  end
end
! { dg-final { scan-tree-dump-not "_gfortran_size" "original" } } 
