! { dg-do compile }
! Tests the fix for PR25072, in which non-PURE functions could
! be referenced inside a FORALL mask.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
module foo
  integer, parameter :: n = 4
contains
  logical function foot (i)
    integer, intent(in) :: i
    foot = (i == 2) .or. (i == 3)
  end function foot
end module foo

  use foo
  integer :: i, a(n)
  logical :: s(n)

  a = 0
  forall (i=1:n, foot (i)) a(i) = i  ! { dg-error "non-PURE" }
  if (any (a .ne. (/0,2,3,0/))) call abort ()

  forall (i=1:n, s (i) .or. t(i)) a(i) = i  ! { dg-error "non-PURE|LOGICAL" }
  if (any (a .ne. (/0,3,2,1/))) call abort ()

  a = 0
  forall (i=1:n, mod (i, 2) == 0) a(i) = w (i)  ! { dg-error "non-PURE" }
  if (any (a .ne. (/0,2,0,4/))) call abort ()

contains
  logical function t(i)
    integer, intent(in) :: i
    t = (mod (i, 2) == 0)
  end function t
  integer function w(i)
    integer, intent(in) :: i
    w = 5 - i
  end function w
end
! { dg-final { cleanup-modules "foo" } }
