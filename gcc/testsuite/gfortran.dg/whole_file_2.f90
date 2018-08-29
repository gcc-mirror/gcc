! { dg-do compile }
! { dg-options "" }
! Tests the fix for PR26227 in which the interface mismatches
! below were not detected.
!
! Contributed by Andrew Pinski <pinskia@gcc.gnu.org>
!
function a(b)
REAL ::b
b = 2.0
a = 1.0
end function

program gg
real :: h
character (5) :: chr = 'hello'
h = a(); ! { dg-warning "Missing actual argument" }
call test ([chr]) ! { dg-warning "Rank mismatch" }
end program gg

subroutine test (a)
  character (5) :: a
  if (a .ne. 'hello') STOP 1
end subroutine test

