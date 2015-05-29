! { dg-do run }
! { dg-options "-fdump-tree-original" }
! PR fortran/36462
!
  implicit none
  character(len=10,kind=1) string1
  character(len=10,kind=4) string4
  string1 = 'ABCDEEDCBA'
  string4 = 'ABCDEEDCBA'

  if(index(string1,1_'A') /= 1) call abort()
  if(index(string4,4_'A') /= 1) call abort()
  if(index(string1,1_'A',kind=4) /= 1_4) call abort()
  if(index(string4,4_'A',kind=4) /= 1_4) call abort()
  if(index(string1,1_'A',kind=1) /= 1_1) call abort()
  if(index(string4,4_'A',kind=1) /= 1_1) call abort()

  if(index(string1,1_'A',back=.true.) /= 10) call abort()
  if(index(string4,4_'A',back=.true.) /= 10) call abort()
  if(index(string1,1_'A',kind=4,back=.true.) /= 10_4) call abort()
  if(index(string4,4_'A',kind=4,back=.true.) /= 10_4) call abort()
  if(index(string1,1_'A',kind=1,back=.true.) /= 10_1) call abort()
  if(index(string4,4_'A',kind=1,back=.true.) /= 10_1) call abort()

  if(index(string1,1_'A',back=.false.) /= 1) call abort()
  if(index(string4,4_'A',back=.false.) /= 1) call abort()
  if(index(string1,1_'A',kind=4,back=.false.) /= 1_4) call abort()
  if(index(string4,4_'A',kind=4,back=.false.) /= 1_4) call abort()
  if(index(string1,1_'A',kind=1,back=.false.) /= 1_1) call abort()
  if(index(string4,4_'A',kind=1,back=.false.) /= 1_1) call abort()

  if(scan(string1,1_'A') /= 1) call abort()
  if(scan(string4,4_'A') /= 1) call abort()
  if(scan(string1,1_'A',kind=4) /= 1_4) call abort()
  if(scan(string4,4_'A',kind=4) /= 1_4) call abort()
  if(scan(string1,1_'A',kind=1) /= 1_1) call abort()
  if(scan(string4,4_'A',kind=1) /= 1_1) call abort()

  if(scan(string1,1_'A',back=.true.) /= 10) call abort()
  if(scan(string4,4_'A',back=.true.) /= 10) call abort()
  if(scan(string1,1_'A',kind=4,back=.true.) /= 10_4) call abort()
  if(scan(string4,4_'A',kind=4,back=.true.) /= 10_4) call abort()
  if(scan(string1,1_'A',kind=1,back=.true.) /= 10_1) call abort()
  if(scan(string4,4_'A',kind=1,back=.true.) /= 10_1) call abort()

  if(scan(string1,1_'A',back=.false.) /= 1) call abort()
  if(scan(string4,4_'A',back=.false.) /= 1) call abort()
  if(scan(string1,1_'A',kind=4,back=.false.) /= 1_4) call abort()
  if(scan(string4,4_'A',kind=4,back=.false.) /= 1_4) call abort()
  if(scan(string1,1_'A',kind=1,back=.false.) /= 1_1) call abort()
  if(scan(string4,4_'A',kind=1,back=.false.) /= 1_1) call abort()
  end

! { dg-final { scan-tree-dump-times "if ..integer.kind=1.. _gfortran_string_index" 6 "original" } }
! { dg-final { scan-tree-dump-times "if ..integer.kind=1.. _gfortran_string_scan" 6 "original" } }
