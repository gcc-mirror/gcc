! { dg-do run }
!
! PR fortran/51055
! PR fortran/49110
!
!
program test
  implicit none
  character(len=:), allocatable :: str
  integer :: i
  i = 5
  str = f()
  call printIt ()
  i = 7
  str = repeat('X', i)
  call printIt ()
contains
  function f()
    character(len=i) :: f
    f = '1234567890'
  end function f
  subroutine printIt
!    print *, len(str)
!    print '(3a)', '>',str,'<'
    if (i == 5) then
      if (str /= "12345" .or. len(str) /= 5) STOP 1
    else if (i == 7) then
      if (str /= "XXXXXXX" .or. len(str) /= 7) STOP 2
    else
      STOP 3
    end if
  end subroutine
end
