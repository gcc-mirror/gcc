! { dg-do compile }
!
! Test the fix for PR90352.
!
! Contributed by Thomas Koenig  <tkoenig@gcc.gnu.org>
!
subroutine bar(c,d) BIND(C) ! { dg-error "must be length 1" }
  character (len=*) c
  character (len=2) d
end
