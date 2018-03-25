! { dg-do run }
! { dg-additional-options "-fcoarray=single -fdump-tree-original" }
!
! Check that {L,U}{,CO}BOUND intrinsics are properly simplified.
!
  implicit none

  type :: t
    integer :: c
  end type t

  type(t) :: d(3:8) = t(7)
  type(t) :: e[5:9,-1:*]
  type(t) :: h(3), j(4), k(0)

  !Test full arrays vs subarrays
  if (lbound(d,      1) /= 3) STOP 1
  if (lbound(d(3:5), 1) /= 1) STOP 2
  if (lbound(d%c,    1) /= 1) STOP 3
  if (ubound(d,      1) /= 8) STOP 4
  if (ubound(d(3:5), 1) /= 3) STOP 5
  if (ubound(d%c,    1) /= 6) STOP 6

  if (lcobound(e,   1) /=  5) STOP 7
  if (lcobound(e%c, 1) /=  5) STOP 8
  if (lcobound(e,   2) /= -1) STOP 9
  if (lcobound(e%c, 2) /= -1) STOP 10
  if (ucobound(e,   1) /=  9) STOP 11
  if (ucobound(e%c, 1) /=  9) STOP 12
  ! no simplification for ucobound(e{,%c}, dim=2)

  if (any(lbound(d     ) /= [3])) STOP 13
  if (any(lbound(d(3:5)) /= [1])) STOP 14
  if (any(lbound(d%c   ) /= [1])) STOP 15
  if (any(ubound(d     ) /= [8])) STOP 16
  if (any(ubound(d(3:5)) /= [3])) STOP 17
  if (any(ubound(d%c   ) /= [6])) STOP 18

  if (any(lcobound(e  ) /=  [5, -1])) STOP 19
  if (any(lcobound(e%c) /=  [5, -1])) STOP 20
  ! no simplification for ucobound(e{,%c})

  call test_empty_arrays(h, j, k)

contains
  subroutine test_empty_arrays(a, c, d)
    type(t) :: a(:), c(-3:0), d(3:1)
    type(t) :: f(4:2), g(0:6)

    if (lbound(a, 1) /=  1) STOP 21
    if (lbound(c, 1) /= -3) STOP 22
    if (lbound(d, 1) /=  1) STOP 23
    if (lbound(f, 1) /=  1) STOP 24
    if (lbound(g, 1) /=  0) STOP 25

    if (ubound(c, 1) /=  0) STOP 26
    if (ubound(d, 1) /=  0) STOP 27
    if (ubound(f, 1) /=  0) STOP 28
    if (ubound(g, 1) /=  6) STOP 29

    if (any(lbound(a) /= [ 1])) STOP 30
    if (any(lbound(c) /= [-3])) STOP 31
    if (any(lbound(d) /= [ 1])) STOP 32
    if (any(lbound(f) /= [ 1])) STOP 33
    if (any(lbound(g) /= [ 0])) STOP 34

    if (any(ubound(c) /= [0])) STOP 35
    if (any(ubound(d) /= [0])) STOP 36
    if (any(ubound(f) /= [0])) STOP 37
    if (any(ubound(g) /= [6])) STOP 38

  end subroutine
end
! { dg-final { scan-tree-dump-not "_gfortran_stop" "original" } }
