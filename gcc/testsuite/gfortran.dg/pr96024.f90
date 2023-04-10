! { dg-do compile }
! PR fortran/96024 - ICE in mio_name_expr_t
! Contributed by G.Steinmetz

module m
  implicit none
  type t
     character(char(1)) :: a ! { dg-error "must be of INTEGER type" }
  end type
  type(t) :: z = t('a')
end
