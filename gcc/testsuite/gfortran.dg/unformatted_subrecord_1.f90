! { dg-do run }
! { dg-options "-fmax-subrecord-length=16" }
! Test Intel record markers with 16-byte subrecord sizes.
program main
  implicit none
  integer, dimension(20) :: n
  integer, dimension(30) :: m
  integer :: i
  real :: r
  integer :: k
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
       256, -16, 16, 289, 324, 361, 400, -16 /))) call abort
  close (10)
  open (10, file="f10.dat", form="unformatted", &
       access="sequential")
  m = 42
  read (10) m(1:5)
  if (any(m(1:5) .ne. (/ 1, 4, 9, 16, 25 /))) call abort
  if (any(m(6:30) .ne. 42)) call abort
  backspace 10
  n = 0
  read (10) n(1:5)
  if (any(n(1:5) .ne. (/ 1, 4, 9, 16, 25 /))) call abort
  if (any(n(6:20) .ne. 0)) call abort
  ! Append to the end of the file
  write (10) 3.14
  ! Test multiple backspace statements
  backspace 10
  backspace 10
  read (10) k
  if (k .ne. 1) call abort
  read (10) r
  if (abs(r-3.14) .gt. 1e-7) call abort
  close (10, status="delete")
end program main
