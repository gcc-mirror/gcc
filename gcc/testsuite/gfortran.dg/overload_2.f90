! { dg-do compile }
! Test the fix for PR32157, in which overloading 'LEN', as
! in 'test' below would cause a compile error.
!
! Contributed by Michael Richmond <michael.a.richmond@nasa.gov>
!
subroutine len(c)
  implicit none
  character :: c
  c = "X"
end subroutine len

subroutine test()
  implicit none
  character :: str
  external len
  call len(str)
  if(str /= "X") STOP 1
end subroutine test

PROGRAM VAL
 implicit none
 external test
 intrinsic len
 call test()
 if(len(" ") /= 1) STOP 2
END
