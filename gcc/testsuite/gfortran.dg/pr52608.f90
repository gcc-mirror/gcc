! { dg-do run }
! PR 52608
! Testcase reduced from NIST testsuite FM110
program fm110_snippet
  implicit none
  real :: aavs
  character(len=100) :: s(2), s2(2)
  AAVS = .087654
35043 FORMAT (" ",16X,"COMPUTED: ",22X,1P/26X,F5.4,3X,2P,F5.3,+3P," ",&
           (23X,F6.2),3X)
5043 FORMAT (17X,"CORRECT:  ",/24X,&
          "  .8765   8.765                         87.65")
  WRITE (s,35043) AAVS,AAVS,AAVS
  WRITE (s2,5043)
  if (s(2) /= s2(2)) call abort()
end program fm110_snippet

