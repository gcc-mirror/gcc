! { dg-do run }
! { dg-options "-fdump-tree-original" }
! PR fortran/36462
!
  implicit none
  character(len=10,kind=1) string1
  character(len=10,kind=4) string4
  string1 = 'ABCDEEDCBA'
  string4 = 'ABCDEEDCBA'

  if(index(string1,1_'A') /= 1) STOP 1
  if(index(string4,4_'A') /= 1) STOP 2
  if(index(string1,1_'A',kind=4) /= 1_4) STOP 3
  if(index(string4,4_'A',kind=4) /= 1_4) STOP 4
  if(index(string1,1_'A',kind=1) /= 1_1) STOP 5
  if(index(string4,4_'A',kind=1) /= 1_1) STOP 6

  if(index(string1,1_'A',back=.true.) /= 10) STOP 7
  if(index(string4,4_'A',back=.true.) /= 10) STOP 8
  if(index(string1,1_'A',kind=4,back=.true.) /= 10_4) STOP 9
  if(index(string4,4_'A',kind=4,back=.true.) /= 10_4) STOP 10
  if(index(string1,1_'A',kind=1,back=.true.) /= 10_1) STOP 11
  if(index(string4,4_'A',kind=1,back=.true.) /= 10_1) STOP 12

  if(index(string1,1_'A',back=.false.) /= 1) STOP 13
  if(index(string4,4_'A',back=.false.) /= 1) STOP 14
  if(index(string1,1_'A',kind=4,back=.false.) /= 1_4) STOP 15
  if(index(string4,4_'A',kind=4,back=.false.) /= 1_4) STOP 16
  if(index(string1,1_'A',kind=1,back=.false.) /= 1_1) STOP 17
  if(index(string4,4_'A',kind=1,back=.false.) /= 1_1) STOP 18

  if(scan(string1,1_'A') /= 1) STOP 19
  if(scan(string4,4_'A') /= 1) STOP 20
  if(scan(string1,1_'A',kind=4) /= 1_4) STOP 21
  if(scan(string4,4_'A',kind=4) /= 1_4) STOP 22
  if(scan(string1,1_'A',kind=1) /= 1_1) STOP 23
  if(scan(string4,4_'A',kind=1) /= 1_1) STOP 24

  if(scan(string1,1_'A',back=.true.) /= 10) STOP 25
  if(scan(string4,4_'A',back=.true.) /= 10) STOP 26
  if(scan(string1,1_'A',kind=4,back=.true.) /= 10_4) STOP 27
  if(scan(string4,4_'A',kind=4,back=.true.) /= 10_4) STOP 28
  if(scan(string1,1_'A',kind=1,back=.true.) /= 10_1) STOP 29
  if(scan(string4,4_'A',kind=1,back=.true.) /= 10_1) STOP 30

  if(scan(string1,1_'A',back=.false.) /= 1) STOP 31
  if(scan(string4,4_'A',back=.false.) /= 1) STOP 32
  if(scan(string1,1_'A',kind=4,back=.false.) /= 1_4) STOP 33
  if(scan(string4,4_'A',kind=4,back=.false.) /= 1_4) STOP 34
  if(scan(string1,1_'A',kind=1,back=.false.) /= 1_1) STOP 35
  if(scan(string4,4_'A',kind=1,back=.false.) /= 1_1) STOP 36
  end

! { dg-final { scan-tree-dump-times "if ..integer.kind=1.. _gfortran_string_index" 6 "original" } }
! { dg-final { scan-tree-dump-times "if ..integer.kind=1.. _gfortran_string_scan" 6 "original" } }
