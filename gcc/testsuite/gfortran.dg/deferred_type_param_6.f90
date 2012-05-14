! { dg-do run }
!
! PR fortran/51055
! PR fortran/49110
!

subroutine test()
  implicit none
  integer :: i = 5
  character(len=:), allocatable :: s1
  call sub(s1, i)
  if (len(s1) /= 5) call abort()
  if (s1 /= "ZZZZZ") call abort()
contains
  subroutine sub(str,j)
    character(len=:), allocatable :: str
    integer :: j
    str = REPEAT("Z",j)
    if (len(str) /= 5) call abort()
    if (str /= "ZZZZZ") call abort()
  end subroutine sub
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
