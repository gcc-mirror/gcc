! { dg-do compile }
!
! Test the fix for PR90352.
!
! Contributed by Thomas Koenig  <tkoenig@gcc.gnu.org>
!
subroutine bar(c,d) BIND(C) ! { dg-error "character dummy argument 'c' at .1. with assumed length is not yet supported for procedure 'bar' with BIND\\(C\\) attribute" }
                            ! { dg-error "Character dummy argument 'd' at .1. must be of constant length of one or assumed length, unless it has assumed shape or assumed rank, as procedure 'bar' has the BIND\\(C\\) attribute" "" { target *-*-* } .-1 }
  character (len=*) c
  character (len=2) d
end
