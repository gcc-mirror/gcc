! { dg-do compile }
! Error recovery on invalid CLASS(), PARAMETER declarations
! PR fortran/103137
! PR fortran/103138
! PR fortran/103693
! PR fortran/105243
! Contributed by G.Steinmetz

program p
  type t
     character(3) :: c = '(a)'
  end type
  class(t), parameter :: x = 1.  ! { dg-error "PARAMETER attribute" }
  class(*), parameter :: y = t() ! { dg-error "PARAMETER attribute" }
  class(*), parameter :: z = 1   ! { dg-error "PARAMETER attribute" }
  print x%c                      ! { dg-error "Syntax error" }
end
