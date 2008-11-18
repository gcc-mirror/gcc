! { dg-do run }
! PR 38135 - pad to RESHAPE didn't work correctly when SOURCE
! was an empty array.

program main
  implicit none
  integer, parameter :: N = 3
  integer(kind=1) :: A1(N,N)
  integer(kind=1) :: b1(n+1)
  integer(kind=4) :: A4(n,n)
  integer(kind=4) :: b4(n+1)
  character(len=9) :: line

  b1 = (/ 1, 2, 2, 2 /)

  A1(1:N,1:N)=reshape(A1(1:0,1),(/N,N/),b1)
  write(unit=line,fmt='(100i1)') A1
  if (line .ne. "122212221") call abort

  b4 = (/ 3, 4, 4, 4 /)

  a4 = reshape(a4(:0,1),(/n,n/),b4)
  write(unit=line,fmt='(100i1)') a4
  if (line .ne. "344434443") call abort
end program main
