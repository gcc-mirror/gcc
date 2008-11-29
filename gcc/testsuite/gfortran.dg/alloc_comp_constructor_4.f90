! { dg-do run }
! Tests the fix for PR32795, which was primarily about memory leakage is
! certain combinations of alloctable components and constructors. This test
! which appears in comment #2 of the PR has the advantage of a wrong
! numeric result which is symptomatic.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
  type :: a
    integer, allocatable :: i(:)
  end type a
  type(a) :: x, y
  x = a ([1, 2, 3])
  y = a (x%i(:))  ! used to cause a memory leak and wrong result
  if (any (x%i .ne. [1, 2, 3])) call abort
end
