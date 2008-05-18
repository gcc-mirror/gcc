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

  if (i /= i1) call abort
  if (i /= i2) call abort
  if (i /= i3) call abort
  if (i /= i4) call abort

  if (iachar (s1) /= i) call abort
  if (iachar (s4) /= i) call abort
  
  if (ichar (s1) /= i) call abort
  if (ichar (s4) /= i) call abort
  
  if (achar(i, kind=1) /= s1) call abort
  if (achar(i, kind=4) /= s4) call abort

  if (char(i, kind=1) /= s1) call abort
  if (char(i, kind=4) /= s4) call abort

  if (iachar(achar(i, kind=1)) /= i) call abort
  if (iachar(achar(i, kind=4)) /= i) call abort

  if (ichar(char(i, kind=1)) /= i) call abort
  if (ichar(char(i, kind=4)) /= i) call abort

end subroutine test

subroutine test_bis (s4, i, i2, i4)
  character(kind=4,len=1) :: s4
  integer :: i, i2, i4

  if (i /= i2) call abort
  if (i /= i4) call abort

  if (iachar (s4) /= i) call abort
  if (ichar (s4) /= i) call abort
  if (achar(i, kind=4) /= s4) call abort
  if (char(i, kind=4) /= s4) call abort
  if (iachar(achar(i, kind=4)) /= i) call abort
  if (ichar(char(i, kind=4)) /= i) call abort

end subroutine test_bis

end
