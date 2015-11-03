! { dg-do run }
! { dg-add-options ieee }
!
! PR fortran/56743
!
! Contributed by Kai Gallmeister
!
! Note that Fortran 2008 (Section 10.11.3.6) requires that there is
! a value separator between the value and the "!".  Thus, all examples
! in this file are invalid; they should either be accepted as vendor
! extension or lead to a run-time error (iostat /=0).
!
! For the c1 and c2 character example, please note that the Fortran
! standard (F2008, 10.11.3.3) requires delimiters; accepting
! a single word (in spirit of list-directed I/O) would be possible
! as vendor extension. But the current run-time failure is fine as well.
!
! Note: After fixing this, warning or error is given with -pedantic -std=xxx
implicit none
integer :: i = -1
real :: r1 = -2
real :: r2 = -3
real :: r3 = -4
real :: r4 = -5
real :: r5 = -6
complex :: c = (-7,-7)
logical :: ll = .false.
character :: c1 = 'X'
character(3) :: c2 = 'YYY'
character(3) :: c3 = 'ZZZ'
namelist /nml/ i, r1,r2,r3,r4,r5,c,ll,c1,c2,c3

open (99, file='nml_87.dat', status="replace")
write(99,*) "&nml"
write(99,*) "  i=42!11"         ! Fixed BUG: wrong result: Unmodified, no error
write(99,*) "  r1=43!11"        ! Fixed BUG: wrong result: Unmodified, no error
write(99,*) "  r2=43.!11"       ! Fixed BUG: wrong result: Unmodified, no error
write(99,*) "  r3=inf!11"       ! OK:  run-time error (Cannot match namelist object)
write(99,*) "  r4=NaN(0x33)!11" ! OK:  run-time error (Cannot match namelist object)
write(99,*) "  r5=3.e5!11"      ! Fixed BUG: wrong result: Unmodified, no error
write(99,*) "  c=(4,2)!11"      ! OK:  value accepted as vendor extension
write(99,*) "  ll=.true.!11"    ! OK:  value accepted as vendor extension
write(99,*) "  c1='a'!11"       ! OK:  without quotes, run-time error (Cannot match namelist object)
write(99,*) "  c2='bc'!11"      ! OK:  without quotes, run-time error (Cannot match namelist object)
write(99,*) "  c3='ax'!11"      ! OK:  without quotes, run-time error (Cannot match namelist object)
write(99,*) "/"

rewind(99)
read (99, nml=nml)
!write (*, nml=nml)
close (99, status="delete")

  if (r1 /= 43) call abort ()
  if (r2 /= 43) call abort ()
  if (r3 /= r3 .or. r3 <= huge(r3)) call abort ()
  if (r4 == r4) call abort ()
  if (r5 /= 300000) call abort ()
  if (c /= cmplx(4,2)) call abort ()
  if (.not. ll) call abort ()
  if (c1 /= "a") call abort ()
  if (c2 /= "bc") call abort ()
  if (c3 /= "ax") call abort ()
end
