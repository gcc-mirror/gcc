! { dg-do compile }
!
! PR fortran/45916
! ICE with generic type-bound operator

module m_sort
  implicit none
  type, abstract :: sort_t
  contains
    generic :: operator(.gt.) => gt_cmp
    procedure(gt_cmp), deferred :: gt_cmp
  end type sort_t
  interface
    logical function gt_cmp(a,b)
      import
      class(sort_t), intent(in) :: a, b
    end function gt_cmp
  end interface
end module m_sort
