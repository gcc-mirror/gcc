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
if(str /= '1bcde') call abort()
call sub(str,len(str))
if(str /= '1bcde') call abort()
call subOUT(str,len(str))
if (len(str) /= 5) call abort()
if(allocated(str)) call abort()
contains
  subroutine sub(x,n)
     integer :: n
     character(len=n), allocatable :: x
     if(len(x) /= 5) call abort()
     if(x /= '1bcde') call abort()
  end subroutine sub
  subroutine subOUT(x,n)
     integer :: n
     character(len=n), allocatable,intent(out) :: x
     if(allocated(x)) call abort()
     if(len(x) /= 5) call abort()
  end subroutine subOUT
end
