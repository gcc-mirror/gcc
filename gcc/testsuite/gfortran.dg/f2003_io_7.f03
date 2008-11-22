! { dg-do run { target fd_truncate } }
! Test case prepared by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
! Test of sign=, decimal=, and blank= .
program iotests
  implicit none
  character(len=45) :: a
  character(len=4) :: mode = "what"
  real, parameter :: pi = 3.14159265358979323846
  real(kind=8), dimension(3) :: b
  !
  write(a,'(f10.3,s,f10.3,sp,f10.3,ss,f10.3)',SIGN='PLUS') pi, pi, pi, pi
  if (a /= "    +3.142     3.142    +3.142     3.142") call abort
  !
  open(8,sign="plus")
  write(8,'(f10.3,dc,f10.3,dp,f10.3)',DECIMAL='COMMA',&
        & sign="suppress") pi, pi, pi
  rewind(8)
  read(8,'(a)') a
  if (a /= "     3,142     3,142     3.142") call abort
  close(8,status="delete")
  !
  !              "123456789 123456789  12345678901
  write(a,'(a)') "53 256.84, 2 2 2.  ; 33.3 3   1  "
  read(a, '(f9.2,1x,f8.2,2x,f11.7)', blank="zero") b(1),b(2),b(3)
  if (any(abs(b - [530256.84, 20202.00, 33.3030001]) > .03)) call abort
end program iotests

