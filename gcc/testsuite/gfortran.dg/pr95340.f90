! { dg-do compile }
! PR fortran/95340 - ICE in gfc_match_select_rank, at fortran/match.c:6690

program p
  type t
  end type t
  class(t) :: z   ! { dg-error "must be dummy, allocatable or pointer" }
  select rank (z) ! { dg-error "must be an assumed rank variable" }
  end select      ! { dg-error "Expecting END PROGRAM" }
end
