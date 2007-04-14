! { dg-do run }
! { dg-options -O2 }
! Tests the fix for PRs29507 and 31404, where elemental functions in
! initialization expressions could not be simplified with array arguments.
!
! Contributed by Steve Kargl <kargl@gcc.gnu.org >
!             and Vivek Rao <vivekrao4@yahoo.com>
!
  real, parameter :: a(2,2) = reshape ((/1.0, 2.0, 3.0, 4.0/), (/2,2/))
  real, parameter :: b(2,2) = sin (a)
  character(8), parameter :: oa(1:3)=(/'nint()  ', 'log10() ', 'sqrt()  '/)
  integer, parameter :: ob(1:3) = index(oa, '(')
  character(6), parameter :: ch(3) = (/"animal", "person", "mantee"/)
  character(1), parameter :: ch2(3) = (/"n", "r", "t"/)
  integer, parameter :: i(3) = index (ch, ch2)
  integer :: ic(1) = len_trim((/"a"/))

  if (any (reshape (b, (/4/)) .ne. (/(sin(real(k)), k = 1,4)/))) call abort ()
  if (any (ob .ne. (/5,6,5/))) call abort ()  ! Original PR29507
  if (any (i .ne. (/2,3,4/))) call abort ()
  if (ic(1) .ne. 1) call abort ()             ! Original PR31404
end
