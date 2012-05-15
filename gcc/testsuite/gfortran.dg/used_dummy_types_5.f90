! { dg-do compile }
! This checks that the fix for PR19362 has not broken gfortran
! in respect of.references allowed by 4.4.2.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!==============
module global

  TYPE :: seq_type1
    sequence
    integer :: i
  end type seq_type1

  TYPE :: nonseq_type1
    integer :: i = 44
  end type nonseq_type1
  type (nonseq_type1), save :: ns1

end module global

  use global, only: seq_type2=>seq_type1, nonseq_type1, ns1

! Host non-sequence types
  type :: different_type
    integer :: i
  end type different_type

  type :: same_type
    sequence
    integer :: i
  end type same_type

  type (seq_type2)  :: t1
  type (different_type)  :: dt1

  type (nonseq_type1) :: ns2
  type (same_type)  :: st1
  real seq_type1

  t1 = seq_type2 (42)
  dt1 = different_type (43)
  ns2 = ns1
  seq_type1 =1.0e32
  st1%i = 45

  call foo (t1)

contains

  subroutine foo (x)

    use global, only: seq_type3=>seq_type1

    TYPE :: seq_type1
      sequence
      integer :: i
    end type seq_type1

    type :: different_type
      complex :: z
    end type different_type

    type :: same_type
      sequence
      integer :: i
    end type same_type
! Host association of renamed type.
    type (seq_type2) :: x
! Locally declared version of the same thing.
    type (seq_type1) :: y
! USE associated renamed type.
    type (seq_type3) :: z


    type (different_type)  :: dt2
    type (same_type)  :: st2

    dt2%z = (2.0,-1.0)
    y = seq_type2 (46)
    z = seq_type3 (47)
    st2 = st1
    print *, x, y, z, dt2, st2, ns2, ns1
  end subroutine foo
END
