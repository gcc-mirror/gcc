! { dg-do run }
! { dg-options "-fdump-tree-original " }
! Checks the fix for PR46896, in which the optimization that passes
! the argument of TRANSPOSE directly missed the possible aliasing
! through host association.
!
! Contributed by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
!
module mod
  integer :: b(2,3) = reshape([1,2,3,4,5,6], [2,3])
contains
  subroutine msub(x)
    integer :: x(:,:)
    b(1,:) = 99
    b(2,:) = x(:,1)
    if (any (b(:,1) /= [99, 1]).or.any (b(:,2) /= [99, 3])) STOP 1
  end subroutine msub
  subroutine pure_msub(x, y)
    integer, intent(in) :: x(:,:)
    integer, intent(OUT) :: y(size (x, 2), size (x, 1))
    y = transpose (x)
  end subroutine pure_msub
end

  use mod
  integer :: a(2,3) = reshape([1,2,3,4,5,6], [2,3])
  call impure
  call purity
contains
!
! pure_sub and pure_msub could be PURE, if so declared.  They do not
! need a temporary.
!
  subroutine purity
    integer :: c(2,3)
    call pure_sub(transpose(a), c)
    if (any (c .ne. a)) STOP 1
    call pure_msub(transpose(b), c)
    if (any (c .ne. b)) STOP 2
  end subroutine purity
!
! sub and msub both need temporaries to avoid aliasing.
!
  subroutine impure
    call sub(transpose(a))
  end subroutine impure

  subroutine sub(x)
    integer :: x(:,:)
    a(1,:) = 88
    a(2,:) = x(:,1)
    if (any (a(:,1) /= [88, 1]).or.any (a(:,2) /= [88, 3])) STOP 2
  end subroutine sub
  subroutine pure_sub(x, y)
    integer, intent(in) :: x(:,:)
    integer, intent(OUT) :: y(size (x, 2), size (x, 1))
    y = transpose (x)
  end subroutine pure_sub
end
!
! The check below for temporaries gave 14 and 33 for "parm" and "atmp".
!
! { dg-final { scan-tree-dump-times "parm" 72 "original" } }
! { dg-final { scan-tree-dump-times "atmp" 13 "original" } }
