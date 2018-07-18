! { dg-do run { xfail i?86-*-freebsd* } }
! { dg-add-options ieee }
! { dg-skip-if "PR libfortran/58015" { hppa*-*-hpux* } }
! { dg-skip-if "IBM long double 31 bits of precision, test requires 38" { powerpc*-*-linux* } }
!
! PR fortran/35862
!
! Test whether I/O rounding works. Uses internally (libgfortran) strtod
! for the conversion - and sets the CPU rounding mode accordingly.
!
! Only few strtod implementations currently support rounding. Therefore
! we use a heuristic to determine if the rounding support is available.
! The assumption is that if strtod gives *different* results for up/down
! rounding, then it will give *correct* results for nearest/zero/up/down
! rounding too. And that is what is effectively checked.
!
! If it doesn't work on your system, please check whether strtod handles
! rounding correctly and whether your system is supported in
! libgfortran/config/fpu*.c
!
! Please only add ... run { target { ! { triplets } } } if it is unfixable
! on your target - and a note why (strtod has broken rounding support, etc.)
!
program main
  use iso_fortran_env
  implicit none

  ! The following uses kinds=10 and 16 if available or
  ! 8 and 10 - or 8 and 16 - or 4 and 8.
  integer, parameter :: xp = real_kinds(ubound(real_kinds,dim=1)-1)
  integer, parameter :: qp = real_kinds(ubound(real_kinds,dim=1))

  real(4) :: r4p, r4m, ref4u, ref4d
  real(8) :: r8p, r8m, ref8u, ref8d
  real(xp) :: r10p, r10m, ref10u, ref10d
  real(qp) :: r16p, r16m, ref16u, ref16d
  character(len=20) :: str, round
  logical :: rnd4, rnd8, rnd10, rnd16

  ! Test for which types glibc's strtod function supports rounding
  str = '0.01 0.01 0.01 0.01'
  read (str, *, round='up') r4p, r8p, r10p, r16p
  read (str, *, round='down') r4m, r8m, r10m, r16m
  rnd4 = r4p /= r4m
  rnd8 = r8p /= r8m
  rnd10 = r10p /= r10m
  rnd16 = r16p /= r16m
!  write (*, *) rnd4, rnd8, rnd10, rnd16

  ref4u = 0.100000001_4
  ref8u = 0.10000000000000001_8

  if (xp == 4) then
    ref10u = 0.100000001_xp
  elseif (xp == 8) then
    ref10u = 0.10000000000000001_xp
  else ! xp == 10
    ref10u = 0.1000000000000000000014_xp
  end if

  if (qp == 8) then
    ref16u = 0.10000000000000001_qp
  elseif (qp == 10) then
    ref16u = 0.1000000000000000000014_qp
  else ! qp == 16
    ref16u = 0.10000000000000000000000000000000000481_qp
  end if

 ! ref*d = 9.999999...
 ref4d = nearest (ref4u, -1.0_4)
 ref8d = nearest (ref8u, -1.0_8)
 ref10d = nearest (ref10u, -1.0_xp)
 ref16d = nearest (ref16u, -1.0_qp)

  round = 'up'
  call t()
  if (rnd4  .and. (r4p  /= ref4u  .or. r4m  /= -ref4d))  STOP 1
  if (rnd8  .and. (r8p  /= ref8u  .or. r8m  /= -ref8d))  STOP 2
  if (rnd10 .and. (r10p /= ref10u .or. r10m /= -ref10d)) STOP 3
  if (rnd16 .and. (r16p /= ref16u .or. r16m /= -ref16d)) STOP 4

  round = 'down'
  call t()
  if (rnd4  .and. (r4p  /= ref4d  .or. r4m  /= -ref4u))  STOP 5
  if (rnd8  .and. (r8p  /= ref8d  .or. r8m  /= -ref8u))  STOP 6
  if (rnd10 .and. (r10p /= ref10d .or. r10m /= -ref10u)) STOP 7
  if (rnd16 .and. (r16p /= ref16d .or. r16m /= -ref16u)) STOP 8

  round = 'zero'
  call t()
  if (rnd4  .and. (r4p  /= ref4d  .or. r4m  /= -ref4d))  STOP 9
  if (rnd8  .and. (r8p  /= ref8d  .or. r8m  /= -ref8d))  STOP 10
  if (rnd10 .and. (r10p /= ref10d .or. r10m /= -ref10d)) STOP 11
  if (rnd16 .and. (r16p /= ref16d .or. r16m /= -ref16d)) STOP 12

  round = 'nearest'
  call t()
  if (rnd4  .and. (r4p  /= ref4u  .or. r4m  /= -ref4u))  STOP 13
  if (rnd8  .and. (r8p  /= ref8u  .or. r8m  /= -ref8u))  STOP 14
  if (rnd10 .and. (r10p /= ref10u .or. r10m /= -ref10u)) STOP 15
  if (rnd16 .and. (r16p /= ref16u .or. r16m /= -ref16u)) STOP 16

! Same as nearest (but rounding towards zero if there is a tie
! [does not apply here])
  round = 'compatible'
  call t()
  if (rnd4  .and. (r4p  /= ref4u  .or. r4m  /= -ref4u))  STOP 17
  if (rnd8  .and. (r8p  /= ref8u  .or. r8m  /= -ref8u))  STOP 18
  if (rnd10 .and. (r10p /= ref10u .or. r10m /= -ref10u)) STOP 19
  if (rnd16 .and. (r16p /= ref16u .or. r16m /= -ref16u)) STOP 20
contains
  subroutine t()
!    print *, round
    str = "0.1 0.1 0.1 0.1"
    read (str, *,round=round) r4p, r8p, r10p, r16p
!    write (*, '(*(g0:"  "))') r4p, r8p, r10p, r16p
    str = "-0.1 -0.1 -0.1 -0.1"
    read (str, *,round=round) r4m, r8m, r10m, r16m
!    write (*, *) r4m, r8m, r10m, r16m
  end subroutine t
end program main
