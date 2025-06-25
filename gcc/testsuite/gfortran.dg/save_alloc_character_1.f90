! { dg-do run }
!
! PR fortran/120713
! Check that the length variable of SAVEd allocatable character arrays are
! not initialized at function entry.

program p
  implicit none
  call s(1)
  call s(2)
contains
  subroutine s(i)
    integer, intent(in) :: i
    character(len=:), allocatable, save :: a(:)
    integer :: j
    if (i == 1) then
      allocate(a, source= [ ('x' // achar(ichar('0') + j), j=1,7) ])
    else
      if (len(a) /= 2) error stop 1
      if (any(a /= ['x1','x2','x3','x4','x5','x6','x7'])) error stop 2
    end if
  end subroutine s
end program p
