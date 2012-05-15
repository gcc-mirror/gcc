! { dg-do compile }
!
! PR fortran/41869
!
! Was ICEing while module write of symbol 'vs_str' in m_dom_dom
! because of "len" being private in fox_m_fsys_format.
!
module fox_m_fsys_array_str
contains
  pure function str_vs(vs) result(s)
    character, dimension(:), intent(in) :: vs
    character(len=size(vs)) :: s
    s = transfer(vs, s)
  end function str_vs
  pure function vs_str(s) result(vs)
    character(len=*), intent(in) :: s
    character, dimension(len(s)) :: vs
    vs = transfer(s, vs)
  end function vs_str
end module fox_m_fsys_array_str

module fox_m_fsys_format
  private
  interface str
    module procedure  str_logical_array
  end interface str
  interface len
    module procedure str_logical_array_len
  end interface
  public :: str
contains
  pure function str_logical_array_len(la) result(n)
    logical, dimension(:), intent(in)   :: la
  end function str_logical_array_len
  pure function str_logical_array(la) result(s)
    logical, dimension(:), intent(in)   :: la
    character(len=len(la)) :: s
  end function str_logical_array
  pure function checkFmt(fmt) result(good)
    character(len=*), intent(in) :: fmt
    logical :: good
    good = len(fmt) > 0
  end function checkFmt
end module fox_m_fsys_format

module m_dom_dom
  use fox_m_fsys_array_str, only: str_vs, vs_str
end module m_dom_dom

module FoX_dom
  use fox_m_fsys_format
  use m_dom_dom
end module FoX_dom

use FoX_dom
implicit none
print *, vs_str("ABC")
end
