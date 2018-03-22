! { dg-do run }
! { dg-options "-fdefault-real-10" }
! { dg-require-effective-target fortran_real_10 }
!
! PR 82143: add a -fdefault-real-16 flag
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

real :: r
real(kind=4) :: r4
real(kind=8) :: r8
double precision :: d
if (kind(r4) /= 4) STOP 1
if (kind(r8) /= 8) STOP 2
if (kind(r) /= 10) STOP 3
if (kind(d)  < 10) STOP 4
end
