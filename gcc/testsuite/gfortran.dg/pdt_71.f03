! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Test the second part of the fix for PR103371.
!
! Compiled but gave the wrong result for the component 'z%x%n'.
!
! Contributed by Arseny Solokha  <asolokha@gmx.com>
!
module m1
  implicit none
  type t
    integer :: n
  end type
  type t2
    ! t and t2 must be resolved to types in m1, not components in t2
    type(t) :: t(10) = t(1)
    type(t) :: x = t(1)
    integer :: t2
    type(t2), pointer :: p => NULL()
  end type
end

module m2
  type :: t(tn)
    integer, kind :: tn
    integer(kind=tn) :: n
  end type
  type :: t2(tm)
    integer, kind :: tm
    type(t(tm)) :: x = t(tm)(2*tm)
  end type
end

  call test_m2
contains
  subroutine test_m2
    use m2
    type(t2(KIND (1))) :: z
    print *, kind (z%x%n), z%x%n
  end subroutine
end
! { dg-final { scan-tree-dump-times "Pdtt2_4.1.x.n = 8" 1 "original" } }
! { dg-final { scan-tree-dump-times "z = Pdtt2_4.1" 1 "original" } }
