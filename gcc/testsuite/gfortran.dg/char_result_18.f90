! { dg-do compile }
!
! Tests the fix for PR80657.
!
! Contributed by Vittorio Zecca  <zeccav@gmail.com>
!
function f(x)
implicit character(len(f)) (x) ! { dg-error "Self reference in character length" }
character(len(x)) f
end
