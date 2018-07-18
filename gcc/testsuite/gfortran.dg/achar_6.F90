! { dg-do run }
! { dg-options "-fbackslash" }

#define TEST(x,y,z) \
  call test (x, y, z, iachar(x), iachar(y), ichar(x), ichar(y))

  TEST("a", 4_"a", 97)
  TEST("\0", 4_"\0", 0)
  TEST("\b", 4_"\b", 8)
  TEST("\x80", 4_"\x80", int(z'80'))
  TEST("\xFF", 4_"\xFF", int(z'FF'))

#define TEST2(y,z) \
  call test_bis (y, z, iachar(y), ichar(y))

  TEST2(4_"\u0100", int(z'0100'))
  TEST2(4_"\ufe00", int(z'fe00'))
  TEST2(4_"\u106a", int(z'106a'))
  TEST2(4_"\uff00", int(z'ff00'))
  TEST2(4_"\uffff", int(z'ffff'))

contains

subroutine test (s1, s4, i, i1, i2, i3, i4)
  character(kind=1,len=1) :: s1
  character(kind=4,len=1) :: s4
  integer :: i, i1, i2, i3, i4

  if (i /= i1) STOP 1
  if (i /= i2) STOP 2
  if (i /= i3) STOP 3
  if (i /= i4) STOP 4

  if (iachar (s1) /= i) STOP 5
  if (iachar (s4) /= i) STOP 6
  
  if (ichar (s1) /= i) STOP 7
  if (ichar (s4) /= i) STOP 8
  
  if (achar(i, kind=1) /= s1) STOP 9
  if (achar(i, kind=4) /= s4) STOP 10

  if (char(i, kind=1) /= s1) STOP 11
  if (char(i, kind=4) /= s4) STOP 12

  if (iachar(achar(i, kind=1)) /= i) STOP 13
  if (iachar(achar(i, kind=4)) /= i) STOP 14

  if (ichar(char(i, kind=1)) /= i) STOP 15
  if (ichar(char(i, kind=4)) /= i) STOP 16

end subroutine test

subroutine test_bis (s4, i, i2, i4)
  character(kind=4,len=1) :: s4
  integer :: i, i2, i4

  if (i /= i2) STOP 17
  if (i /= i4) STOP 18

  if (iachar (s4) /= i) STOP 19
  if (ichar (s4) /= i) STOP 20
  if (achar(i, kind=4) /= s4) STOP 21
  if (char(i, kind=4) /= s4) STOP 22
  if (iachar(achar(i, kind=4)) /= i) STOP 23
  if (ichar(char(i, kind=4)) /= i) STOP 24

end subroutine test_bis

end
