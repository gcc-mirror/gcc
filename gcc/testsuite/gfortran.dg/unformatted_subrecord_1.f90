! { dg-do run { target fd_truncate } }
! { dg-options "-fmax-subrecord-length=16" }
! Test Intel record markers with 16-byte subrecord sizes.
! PR 32770:  Use explicit kinds for all integers and constants,
! to avoid problems with -fdefault-integer-8 and -fdefault-real-8
program main
  implicit none
  integer(kind=4), dimension(20) :: n
  integer(kind=4), dimension(30) :: m
  integer(kind=4) :: i
  real(kind=4) :: r
  integer(kind=4) :: k
  ! Maximum subrecord length is 16 here, or the test will fail.
  open (10, file="f10.dat", &
       form="unformatted", access="sequential")
  n = (/ (i**2, i=1, 20) /)
  write (10) n
  close (10)
  ! Read back the file, including record markers.
  open (10, file="f10.dat", form="unformatted", access="stream")
  read (10) m
  if (any(m .ne. (/ -16, 1, 4, 9, 16, 16, -16, 25, 36, 49, 64, &
       -16, -16, 81, 100, 121, 144, -16, -16, 169, 196, 225, & 
       256, -16, 16, 289, 324, 361, 400, -16 /))) STOP 1
  close (10)
  open (10, file="f10.dat", form="unformatted", &
       access="sequential")
  m = 42
  read (10) m(1:5)
  if (any(m(1:5) .ne. (/ 1, 4, 9, 16, 25 /))) STOP 2
  if (any(m(6:30) .ne. 42)) STOP 3
  backspace 10
  n = 0
  read (10) n(1:5)
  if (any(n(1:5) .ne. (/ 1, 4, 9, 16, 25 /))) STOP 4
  if (any(n(6:20) .ne. 0)) STOP 5
  ! Append to the end of the file
  write (10) 3.14_4
  ! Test multiple backspace statements
  backspace 10
  backspace 10
  read (10) k
  if (k .ne. 1) STOP 6
  read (10) r
  if (abs(r-3.14_4) .gt. 1e-7) STOP 7
  close (10, status="delete")
end program main
