! { dg-do compile }
! This checks the fix for PR19362 in which types from different scopes
! that are the same, according to 4.4.2, would generate an ICE if one
! were assigned to the other.  As well as the test itself, various
! other requirements of 4.4.2 are tested here.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!==============
module global

  TYPE :: seq_type1
    sequence
    integer :: i
  end type seq_type1

  TYPE :: nonseq_type1
    integer :: i
  end type nonseq_type1
  type (nonseq_type1) :: ns1

end module global

! Host types with local name != true name
  use global, only: seq_type2=>seq_type1, nonseq_type2=>nonseq_type1, ns1
  type (nonseq_type2) :: ns2

! Host non-sequence types
  type :: different_type
    integer :: i
  end type different_type
  type (different_type)  :: dt1

  type :: same_type
    integer :: i
  end type same_type
  type (same_type)  :: st1

  real :: seq_type1

! Provide a reference to dt1.
  dt1 = different_type (42)
! These share a type declaration.
  ns2 = ns1
! USE associated seq_type1 is renamed.
  seq_type1 = 1.0

! These are different.
  st1 = dt                ! { dg-error "convert REAL" }

  call foo (st1)          ! { dg-error "Type mismatch in argument" }

contains

  subroutine foo (st2)

! Contained type with local name != true name.
! This is the same as seq_type2 in the host.
    use global, only: seq_type3=>seq_type1

! This local declaration is the same as seq_type3 and seq_type2.
    TYPE :: seq_type1
      sequence
      integer :: i
    end type seq_type1

! Host association of renamed type.
    type (seq_type2) :: x
! Locally declared version of the same thing.
    type (seq_type1) :: y
! USE associated renamed type.
    type (seq_type3) :: z

! Contained type that is different to that in the host.
    type :: different_type
      complex :: z
    end type different_type

    type :: same_type
      integer :: i
    end type same_type

    type (different_type)  :: b
    type (same_type)  :: st2

! Error because these are not the same.
    b = dt1               ! { dg-error "convert TYPE" }

! Error in spite of the name - these are non-sequence types and are NOT
! the same.
    st1 = st2             ! { dg-error "convert TYPE" }

    b%z = (2.0,-1.0)

! Check that the references that are correct actually work. These test the
! fix for PR19362.
    x = seq_type1 (1)
    y = x
    y = seq_type3 (99)
  end subroutine foo
END

! { dg-final { cleanup-modules "global" } }
