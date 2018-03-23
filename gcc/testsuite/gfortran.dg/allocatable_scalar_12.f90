! { dg-do run }
!
! PR fortran/47421
!
! Don't auto-deallocatable scalar character allocatables.
!
implicit none
character(len=5), allocatable :: str
allocate(str)
str = '1bcde'
if(str /= '1bcde') STOP 1
call sub(str,len(str))
if(str /= '1bcde') STOP 2
call subOUT(str,len(str))
if (len(str) /= 5) STOP 3
if(allocated(str)) STOP 4
contains
  subroutine sub(x,n)
     integer :: n
     character(len=n), allocatable :: x
     if(len(x) /= 5) STOP 5
     if(x /= '1bcde') STOP 6
  end subroutine sub
  subroutine subOUT(x,n)
     integer :: n
     character(len=n), allocatable,intent(out) :: x
     if(allocated(x)) STOP 7
     if(len(x) /= 5) STOP 8
  end subroutine subOUT
end
