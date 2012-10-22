! { dg-do compile }
! { dg-options "-O2" }
!
! PR fortran/54884
!
! Check that get_key_len is not optimized away as it
! is used in a publicly visible specification expression.
!
module m_common_attrs
  private
  !...
  public :: get_key
contains
  pure function get_key_len() result(n)
    n = 5
  end function get_key_len
  pure function other() result(n)
    n = 5
  end function other
  ! ...
  function get_key() result(key)
    ! ...
    character(len=get_key_len()) :: key
    key = ''
  end function get_key
end module m_common_attrs

! { dg-final { scan-assembler-not "__m_common_attrs_MOD_other" } }
! { dg-final { scan-assembler "__m_common_attrs_MOD_get_key_len" } }
