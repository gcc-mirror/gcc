! { dg-do compile }
module fox_m_fsys_format

  interface len
     module procedure str_real_sp_len, str_real_sp_fmt_len
  end interface

contains

  pure function str_real_sp_fmt_len(x, fmt) result(n)
    real, intent(in) :: x
    character(len=*), intent(in) :: fmt
    if (.not.checkFmt(fmt)) then
    endif
  end function str_real_sp_fmt_len
  pure function str_real_sp_len(x) result(n)
    real, intent(in) :: x
    n = len(x, "")
  end function str_real_sp_len
  pure function str_real_dp_matrix(xa) result(s)
    real, intent(in) :: xa
    character(len=len(xa)) :: s
  end function str_real_dp_matrix

  pure function checkfmt(s) result(a)
   logical a
   character(len=*), intent(in) :: s
  end function checkfmt
end module fox_m_fsys_format
