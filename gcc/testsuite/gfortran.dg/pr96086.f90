! { dg-do compile }
! PR fortran/96086 - ICE in gfc_match_select_rank, at fortran/match.c:6645

subroutine s
  class(*) :: x(..)     ! { dg-error "Assumed-rank array" }
  select rank (y => x)  ! { dg-error "CLASS variable" }
  end select
end
