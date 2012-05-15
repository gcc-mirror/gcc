! { dg-do run }
! { dg-options "-fbackslash" }
!
! Testcase from PR36162
module m
  character(*), parameter ::  a ='H\0z'
end module m

  use m
  character(len=20) :: s
  if (a /= 'H\0z') call abort
  if (ichar(a(2:2)) /= 0) call abort
  write (s,"(A)") a
end
