! pr 15294 - [gfortran] ADJUSTR intrinsic accesses corrupted pointer
!
  program test_adjustr
  implicit none
  integer test_cases
  parameter (test_cases=13)
  integer i
  character(len=10) s1(test_cases), s2(test_cases)
  s1(1)='A'
  s2(1)='         A'
  s1(2)='AB'
  s2(2)='        AB'
  s1(3)='ABC'
  s2(3)='       ABC'
  s1(4)='ABCD'
  s2(4)='      ABCD'
  s1(5)='ABCDE'
  s2(5)='     ABCDE'
  s1(6)='ABCDEF'
  s2(6)='    ABCDEF'
  s1(7)='ABCDEFG'
  s2(7)='   ABCDEFG'
  s1(8)='ABCDEFGH'
  s2(8)='  ABCDEFGH'
  s1(9)='ABCDEFGHI'
  s2(9)=' ABCDEFGHI'
  s1(10)='ABCDEFGHIJ'
  s2(10)='ABCDEFGHIJ'
  s1(11)=''
  s2(11)=''
  s1(12)=' '
  s2(12)=' '
  s1(13)='          '
  s2(13)='          '
  do I = 1,test_cases
     print*,i
     print*, 's1          = "', s1(i), '"'
     print*, 's2          = "', s2(i), '"'
     print*, 'adjustr(s1) = "', adjustr(s1(i)), '"'
     if (adjustr(s1(i)).ne.s2(i)) then
         print*,'fail'
         STOP 1
     endif
  enddo
   
  end program test_adjustr
