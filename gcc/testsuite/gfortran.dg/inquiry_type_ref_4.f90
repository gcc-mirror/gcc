! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Check the fix for PR87881.
!
  complex(8) :: zi = (0,-1_8)
  character(2) :: chr ='ab'
  if (zi%re%kind .ne. kind (real (zi))) stop 1
  if (chr%len%kind .ne. kind (len (chr))) stop 2

! After simplification there should only be the delarations for 'zi' and 'chr'

! { dg-final { scan-tree-dump-times "zi" 1 "original" } }
! { dg-final { scan-tree-dump-times "chr" 1 "original" } }
end
