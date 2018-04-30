! { dg-do run }
! Tests the fix for PR32926, in which the call to fcn
! in bar would cause an ICE because it had not been referenced
! in the namespace where it was declared.
!
! Contributed by Ralph Baker Kearfott <rbk@louisiana.edu>
!
subroutine foobar1
  common // chr
  character(8) :: chr
  chr = "foobar1"
end subroutine
subroutine foobar2
  common // chr
  character(8) :: chr
  chr = "foobar2"
end subroutine

subroutine foo (fcn)
  external fcn
  call bar
contains
  subroutine bar
    call fcn
  end subroutine bar
end subroutine foo

  external foo, foobar1, foobar2
  common // chr
  character(8) :: chr
  call foo (foobar1)
  if (chr .ne. "foobar1") STOP 1
  call foo (foobar2)
  if (chr .ne. "foobar2") STOP 2
end
