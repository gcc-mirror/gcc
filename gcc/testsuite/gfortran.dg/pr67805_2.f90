! { dg-do compile }
! PR fortran/68108
! Code contributed by Juergen Reuter (juergen.reuter at desy dot de)
! Test fix for regression caused by PR fortran/67805.
module lexers
  implicit none
  type :: template_t
     character(256) :: charset1
     integer :: len1
  end type template_t

contains

  subroutine match_quoted (tt, s, n)
    type(template_t), intent(in) :: tt
    character(*), intent(in) :: s
    integer, intent(out) :: n
    character(tt%len1) :: ch1
    ch1 = tt%charset1
  end subroutine match_quoted

end module lexers
