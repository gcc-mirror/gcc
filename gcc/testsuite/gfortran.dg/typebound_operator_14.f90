! { dg-do compile }
!
! PR fortran/52024
!
! The test case was segfaulting before
!

module m_sort
  implicit none
  type, abstract :: sort_t
  contains
    generic :: operator(.gt.) => gt_cmp
    procedure :: gt_cmp
    end type sort_t
contains
  logical function gt_cmp(a,b)
    class(sort_t), intent(in) :: a, b
    gt_cmp = .true.
  end function gt_cmp
end module

module test
  use m_sort
  implicit none
  type, extends(sort_t) :: sort_int_t
    integer :: i
  contains
    generic :: operator(.gt.) => gt_cmp_int ! { dg-error "are ambiguous" }
    procedure :: gt_cmp_int
  end type
contains
  logical function gt_cmp_int(a,b) result(cmp)
    class(sort_int_t), intent(in) :: a, b
    if (a%i > b%i) then
      cmp = .true.
     else
      cmp = .false.
     end if
  end function gt_cmp_int
end module

! { dg-final { cleanup-tree-dump "m_sort test" } }
