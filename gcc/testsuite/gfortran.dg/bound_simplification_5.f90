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
  if (lbound(d,      1) /= 3) call abort
  if (lbound(d(3:5), 1) /= 1) call abort
  if (lbound(d%c,    1) /= 1) call abort
  if (ubound(d,      1) /= 8) call abort
  if (ubound(d(3:5), 1) /= 3) call abort
  if (ubound(d%c,    1) /= 6) call abort  

  if (lcobound(e,   1) /=  5) call abort
  if (lcobound(e%c, 1) /=  5) call abort
  if (lcobound(e,   2) /= -1) call abort
  if (lcobound(e%c, 2) /= -1) call abort
  if (ucobound(e,   1) /=  9) call abort
  if (ucobound(e%c, 1) /=  9) call abort
  ! no simplification for ucobound(e{,%c}, dim=2)

  if (any(lbound(d     ) /= [3])) call abort
  if (any(lbound(d(3:5)) /= [1])) call abort
  if (any(lbound(d%c   ) /= [1])) call abort
  if (any(ubound(d     ) /= [8])) call abort
  if (any(ubound(d(3:5)) /= [3])) call abort
  if (any(ubound(d%c   ) /= [6])) call abort  

  if (any(lcobound(e  ) /=  [5, -1])) call abort
  if (any(lcobound(e%c) /=  [5, -1])) call abort
  ! no simplification for ucobound(e{,%c})

  call test_empty_arrays(h, j, k)

contains
  subroutine test_empty_arrays(a, c, d)
    type(t) :: a(:), c(-3:0), d(3:1)
    type(t) :: f(4:2), g(0:6)

    if (lbound(a, 1) /=  1) call abort
    if (lbound(c, 1) /= -3) call abort
    if (lbound(d, 1) /=  1) call abort
    if (lbound(f, 1) /=  1) call abort
    if (lbound(g, 1) /=  0) call abort

    if (ubound(c, 1) /=  0) call abort
    if (ubound(d, 1) /=  0) call abort
    if (ubound(f, 1) /=  0) call abort
    if (ubound(g, 1) /=  6) call abort

    if (any(lbound(a) /= [ 1])) call abort
    if (any(lbound(c) /= [-3])) call abort
    if (any(lbound(d) /= [ 1])) call abort
    if (any(lbound(f) /= [ 1])) call abort
    if (any(lbound(g) /= [ 0])) call abort

    if (any(ubound(c) /= [0])) call abort
    if (any(ubound(d) /= [0])) call abort
    if (any(ubound(f) /= [0])) call abort
    if (any(ubound(g) /= [6])) call abort

  end subroutine
end
! { dg-final { scan-tree-dump-not "abort" "original" } }
