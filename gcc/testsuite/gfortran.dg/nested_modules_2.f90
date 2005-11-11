! { dg do-run }
! This tests the patch for PR16861.
!
! Contributed by Paul Thomas <pault@gcc.gnu.org>
!
module foo
INTEGER :: i
end module foo

module bar
contains
subroutine sub1 (j)
  use foo
  integer, dimension(i) :: j
  j = 42
end subroutine sub1
subroutine sub2 (k)
  use foo
  integer, dimension(i) :: k
  k = 84
end subroutine sub2
end module bar

module foobar
  use foo                      !This used to cause an ICE
  use bar
end module foobar

program testfoobar
  use foobar
  integer, dimension(3)  :: l = 0
  i = 2
  call sub1 (l)
  i = 1
  call sub2 (l)
  if (all (l.ne.(/84,42,0/))) call abort ()
end program testfoobar

