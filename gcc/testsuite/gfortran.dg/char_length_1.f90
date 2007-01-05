! { dg-do compile }
! { dg-options "-Wall -std=f2003" }
! Tests the patch for PR27996 and PR27998, in which warnings
! or errors were not emitted when the length of character
! constants was changed silently.
!
! Contributed by Tobias Burnus <tobias.burnus@physik.fu-berlin.de> 
!
program test
  character(10) :: a(3)
  character(10) :: b(3)= &
       (/ 'Takata ', 'Tanaka', 'Hayashi' /) ! { dg-error "same length" }
  character(4) :: c = "abcde"  ! { dg-warning "being truncated" }
  a =  (/ 'Takata', 'Tanaka ', 'Hayashi' /) ! { dg-error "same length" }
  a =  (/ 'Takata ', 'Tanaka ', 'Hayashi' /)
  b = "abc"
  c = "abcdefg"   ! { dg-warning "will be truncated" }
end program test
