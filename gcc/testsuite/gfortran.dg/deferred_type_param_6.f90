! { dg-do run }
!
! PR fortran/51055
! PR fortran/49110
! PR fortran/60334

subroutine test()
  implicit none
  integer :: i = 5
  character(len=:), allocatable :: s1
  character(len=:), pointer :: s2
  character(len=5), target :: fifeC = 'FIVEC'
  call sub(s1, i)
  if (len(s1) /= 5) call abort()
  if (s1 /= "ZZZZZ") call abort()
  s2 => subfunc()
  if (len(s2) /= 5) call abort()
  if (s2 /= "FIVEC") call abort()
  s1 = addPrefix(subfunc())
  if (len(s1) /= 7) call abort()
  if (s1 /= "..FIVEC") call abort()
contains
  subroutine sub(str,j)
    character(len=:), allocatable :: str
    integer :: j
    str = REPEAT("Z",j)
    if (len(str) /= 5) call abort()
    if (str /= "ZZZZZ") call abort()
  end subroutine sub
  function subfunc() result(res)
    character(len=:), pointer :: res
    res => fifec
    if (len(res) /= 5) call abort()
    if (res /= "FIVEC") call abort()
  end function subfunc
  function addPrefix(str) result(res)
    character(len=:), pointer :: str
    character(len=:), allocatable :: res
    res = ".." // str
  end function addPrefix
end subroutine test

program a
 character(len=:),allocatable :: s
 integer :: j=2
 s = repeat ('x', j)
 if (len(repeat(' ',j)) /= 2) call abort()
 if (repeat('y',j) /= "yy") call abort()
 if (len(s) /= 2) call abort()
 if (s /= "xx") call abort()
 call test()
end program a
