! { dg-do run }
! { dg-options "-fdefault-real-10" }
!
! PR 82143: add a -fdefault-real-16 flag
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

real :: r
real(kind=4) :: r4
real(kind=8) :: r8
double precision :: d
if (kind(r4) /= 4) call abort
if (kind(r8) /= 8) call abort
if (kind(r) /= 10) call abort
if (kind(d) /= 16) call abort
end
