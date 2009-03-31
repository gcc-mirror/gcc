! { dg-do run }
!
! Test the fix for PR38915 in which the character length of the
! temporaries produced in the assignments marked below was set to
! one.
!
! Contributed by Dick Hendrickson <dick.hendrickson@gmail.com>
!
program cg0033_41
  type t
    sequence
    integer i
    character(len=9) c
  end type t
  type (t)  L(3),R(3), LL(4), RR(4)
  EQUIVALENCE (L,LL)
  integer nfv1(3), nfv2(3)
  R(1)%c = '123456789'
  R(2)%c = 'abcdefghi'
  R(3)%c = '!@#$%^&*('
  L%c = R%c
  LL(1:3)%c = R%c
  LL(4)%c = 'QWERTYUIO'
  RR%c = LL%c            ! The equivalence forces a dependency
  L%c = LL(2:4)%c
  if (any (RR(2:4)%c .ne. L%c)) call abort
  nfv1 = (/1,2,3/)
  nfv2 = nfv1
  L%c = R%c
  L(nfv1)%c = L(nfv2)%c  ! The vector indices force a dependency
  if (any (R%c .ne. L%c)) call abort
end

