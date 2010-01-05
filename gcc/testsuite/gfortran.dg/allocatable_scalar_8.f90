! { dg-do run }
!
! PR fortran/41872
!
! Character functions returning allocatable scalars
!
program test
  implicit none
  if (func () /= 'abc') call abort ()
contains
  function func() result (str)
    character(len=3), allocatable :: str
    if (allocated (str)) call abort ()
    allocate (str)
    str = 'abc'
  end function func
end program test
