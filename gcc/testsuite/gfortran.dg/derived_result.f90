! { dg-do compile }
!
! PR 78593: [6/7 Regression] ICE in gfc_match_varspec, at fortran/primary.c:2053
!
! Contributed by Gerhard Steinmetz <gerhard.steinmetz.fortran@t-online.de>

type(t) function add (x, y)    ! { dg-error "is not accessible" }
  integer, intent(in) :: x, y
  add%a = x + y                ! { dg-error "Unclassifiable statement" }
end
