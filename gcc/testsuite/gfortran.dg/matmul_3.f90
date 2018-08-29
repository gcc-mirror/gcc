! { dg-do run }
! Check the fix for PR28005, in which the mechanism for dealing
! with matmul (transpose (a), b) would cause wrong results for
! matmul (a(i, 1:n), b(1:n, 1:n)).
!
! Based on the original testcase contributed by
! Tobias Burnus  <tobias.burnus@physik.fu-berlin.de>
!   
   implicit none
   integer, parameter         ::  nmax = 3
   integer                    ::  i, n = 2
   integer, dimension(nmax,nmax) ::  iB=0 , iC=1
   integer, dimension(nmax,nmax) ::  iX1=99, iX2=99, iChk
   iChk = reshape((/30,66,102,36,81,126,42,96,150/),(/3,3/))

! This would give 3, 3, 99
   iB = reshape((/1 ,3 ,0 ,2 ,5 ,0 ,0 ,0 ,0 /),(/3,3/))
   iX1(1:n,1) = matmul( iB(2,1:n),iC(1:n,1:n) )

! This would give 4, 4, 99
   ib(3,1) = 1
   iX2(1:n,1) = matmul( iB(2,1:n),iC(1:n,1:n) )

! Whereas, we should have 8, 8, 99
   if (any (iX1(1:n+1,1) .ne. (/8, 8, 99/))) STOP 1
   if (any (iX1 .ne. iX2)) STOP 2

! Make sure that the fix does not break transpose temporaries.
   iB = reshape((/(i, i = 1, 9)/),(/3,3/))
   ic = transpose (iB)
   iX1 = transpose (iB)
   iX1 = matmul (iX1, iC)
   iX2 = matmul (transpose (iB), iC)
   if (any (iX1 .ne. iX2)) STOP 3
   if (any (iX1 .ne. iChk)) STOP 4
end
