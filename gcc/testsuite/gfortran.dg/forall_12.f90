! { dg-do run }
! Tests the fix for PR31217 and PR33811 , in which dependencies were not
! correctly handled for the assignments below and, when this was fixed,
! the last two ICEd on trying to create the temorary.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!              Dominique d'Humieres <dominiq@lps.ens.fr>
!                   and Paul Thomas <pault@gcc.gnu.org>
!
  character(len=1) :: a = "1"
  character(len=1) :: b(4) = (/"1","2","3","4"/), c(4)
  c = b
  forall(i=1:1) a(i:i) = a(i:i)         ! This was the original PR31217
  forall(i=1:1) b(i:i) = b(i:i)         ! The rest were found to be broken
  forall(i=1:1) b(:)(i:i) = b(:)(i:i)
  forall(i=1:1) b(1:3)(i:i) = b(2:4)(i:i)
  if (any (b .ne. (/"2","3","4","4"/))) STOP 1
  b = c
  forall(i=1:1) b(2:4)(i:i) = b(1:3)(i:i)
  if (any (b .ne. (/"1","1","2","3"/))) STOP 2
  b = c
  do i = 1, 1
    b(2:4)(i:i) = b(1:3)(i:i)           ! This was PR33811 and Paul's bit
  end do
  if (any (b .ne. (/"1","1","2","3"/))) STOP 3
  call foo
contains
  subroutine foo
    character(LEN=12) :: a(2) = "123456789012"
    character(LEN=12) :: b = "123456789012"
! These are Dominique's
    forall (i = 3:10) a(:)(i:i+2) = a(:)(i-2:i)
    IF (a(1) .ne. "121234567890") STOP 4
    forall (i = 3:10) a(2)(i:i+2) = a(1)(i-2:i)
    IF (a(2) .ne. "121212345678") STOP 5
    forall (i = 3:10) b(i:i+2) = b(i-2:i)
    IF (b .ne. "121234567890") STOP 6
  end subroutine
end

