!  { dg-do run }
!  Tests the fix for PR35740, where the trick of interchanging the descriptor
!  dimensions to implement TRANSPOSE did not work if it is an argument of
!  an elemental function - eg. CONJG.  The fix forces a library call for such
!  cases.  During the diagnosis of the PR, it was found that the scalarizer was
!  completely thrown if the argument of TRANSPOSE was a non-variable
!  expression; eg a + c below.  This is also fixed by the library call.
!
!  Contributed by Dominik Muth <dominik.muth@gmx.de>
!
program main
  implicit none
  complex, dimension(2,2) :: a,b,c,d
  a(1,1) = (1.,1.)
  a(2,1) = (2.,2.)
  a(1,2) = (3.,3.)
  a(2,2) = (4.,4.)
!
  b = a
  b = conjg(transpose(b))
  d = a
  d = transpose(conjg(d))
  if (any (b /= d)) call abort ()
!
  d = matmul (b,  a )
  if (any (d /= matmul (transpose(conjg(a)), a))) call abort ()
  if (any (d /= matmul (conjg(transpose(a)), a))) call abort ()
!
  c = (0.0,1.0)
  b = conjg(transpose(a + c))
  d = transpose(conjg(a + c))
  if (any (b /= d)) call abort ()
!
  d = matmul (b,  a + c)
  if (any (d /= matmul (transpose(conjg(a + c)), a + c))) call abort ()
  if (any (d /= matmul (conjg(transpose(a + c)), a + c))) call abort ()
 END program main
