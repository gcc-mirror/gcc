! { dg-do compile }
!
! Test the fix for PR90352.
!
! Contributed by Thomas Koenig  <tkoenig@gcc.gnu.org>
!
subroutine bar(c,d) BIND(C) ! { dg-error "character dummy argument 'c' at .1. with assumed length is not yet supported for procedure 'bar' with BIND\\(C\\) attribute" }
  character (len=*) c
  character (len=2) d
end
