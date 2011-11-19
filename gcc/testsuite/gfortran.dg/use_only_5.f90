! { dg-do compile }
!
! PR fortran/39427
!
! Test case was failing with the initial version of the
! constructor patch.
!
! Based on the Fortran XML library FoX

module m_common_attrs
  implicit none
  private

  type dict_item
    integer, allocatable :: i(:)
  end type dict_item

  type dictionary_t
    private
    type(dict_item), pointer :: d => null()
  end type dictionary_t

  public :: dictionary_t
  public :: get_prefix_by_index

contains
  pure function get_prefix_by_index(dict) result(prefix)
    type(dictionary_t), intent(in) :: dict
    character(len=size(dict%d%i)) :: prefix
  end function get_prefix_by_index
end module m_common_attrs

module m_common_namespaces
  use m_common_attrs, only: dictionary_t
  use m_common_attrs, only: get_prefix_by_index
end module m_common_namespaces

! { dg-final { cleanup-modules "m_common_attrs m_common_namespaces" } }
