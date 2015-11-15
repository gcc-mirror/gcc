! { dg-do run }
!
! Tests the fix for pr49954, in which concatenation to deferred length character
! arrays, at best, did not work correctly.
!
!
!
implicit none
  character(len=:), allocatable :: a1(:)
  character(len=:), allocatable :: a2(:), a3(:)
  character(len=:), allocatable :: b1
  character(len=:), allocatable :: b2
  character(8) :: chr = "IJKLMNOP"
  character(48) :: buffer

  a1 = ["ABCDEFGH","abcdefgh"]
  a2 = "_"//a1//chr//"_"
  if (any (a2 .ne. ["_ABCDEFGHIJKLMNOP_","_abcdefghIJKLMNOP_"])) call abort

! Check that the descriptor dtype is OK - the array write needs it.
  write (buffer, "(2a18)") a2
  if (trim (buffer) .ne. "_ABCDEFGHIJKLMNOP__abcdefghIJKLMNOP_") call abort

! Make sure scalars survived the fix!
  b1 = "ABCDEFGH"
  b2 = "_"//b1//chr//"_"
  if (b2 .ne. "_ABCDEFGHIJKLMNOP_") call abort

! Check the dependency is detected and dealt with by generation of a temporary.
  a1 = "?"//a1//"?"
  if (any (a1 .ne. ["?ABCDEFGH?","?abcdefgh?"])) call abort
! With an array reference...
  a1 = "?"//a1(1:2)//"?"
  if (any (a1 .ne. ["??ABCDEFGH??","??abcdefgh??"])) call abort
!... together with a substring.
  a1 = "?"//a1(1:1)(2:4)//"?"
  if (any (a1 .ne. ["??AB?"])) call abort
contains
end
