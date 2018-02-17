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
  if (len(s1) /= 5) STOP 1
  if (s1 /= "ZZZZZ") STOP 2
  s2 => subfunc()
  if (len(s2) /= 5) STOP 3
  if (s2 /= "FIVEC") STOP 4
  s1 = addPrefix(subfunc())
  if (len(s1) /= 7) STOP 5
  if (s1 /= "..FIVEC") STOP 6
contains
  subroutine sub(str,j)
    character(len=:), allocatable :: str
    integer :: j
    str = REPEAT("Z",j)
    if (len(str) /= 5) STOP 7
    if (str /= "ZZZZZ") STOP 8
  end subroutine sub
  function subfunc() result(res)
    character(len=:), pointer :: res
    res => fifec
    if (len(res) /= 5) STOP 9
    if (res /= "FIVEC") STOP 10
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
 if (len(repeat(' ',j)) /= 2) STOP 11
 if (repeat('y',j) /= "yy") STOP 12
 if (len(s) /= 2) STOP 13
 if (s /= "xx") STOP 14
 call test()
end program a
