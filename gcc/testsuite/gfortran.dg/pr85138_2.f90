! { dg-do compile }
module fox_m_fsys_format
  interface len
    module procedure str_real_dp_len, str_real_dp_fmt_len
  end interface
contains
  pure function str_real_dp_fmt_len(x, fmt) result(n)
    real, intent(in) :: x
    character(len=*), intent(in) :: fmt
    if (.not.checkFmt(fmt)) then
    endif
  end function str_real_dp_fmt_len
  pure function str_real_dp_len(x) result(n)
    real, intent(in) :: x
  end function str_real_dp_len
  pure function str_real_dp_array_len(xa) result(n)
    real, dimension(:), intent(in) :: xa
  end function str_real_dp_array_len
  pure function str_real_dp_array_fmt_len(xa, fmt) result(n)
    real, dimension(:), intent(in) :: xa
    character(len=*), intent(in) :: fmt
  end function str_real_dp_array_fmt_len
  pure function str_real_dp_fmt(x, fmt) result(s)
    real, intent(in) :: x
    character(len=*), intent(in) :: fmt
    character(len=len(x, fmt)) :: s
  end function str_real_dp_fmt
  pure function checkFmt(fmt) result(good)
    character(len=*), intent(in) :: fmt
    logical :: good
  end function checkFmt
end module fox_m_fsys_format
