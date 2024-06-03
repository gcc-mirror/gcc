! { dg-do run }
!
! PR fortran/83865
!
! Test ALLOCATE with SOURCE= of deferred length character, where
! the source-expression is an array of character with length 0.

program p
  implicit none
  character(:), allocatable :: z(:)
  character(1) :: cc(4) = ""
  allocate (z, source=[''])
  if (len (z) /= 0 .or. size (z) /= 1) stop 1
  deallocate (z)
  allocate (z, source=['',''])
  if (len (z) /= 0 .or. size (z) /= 2) stop 2
  deallocate (z)
  allocate (z, source=[ character(0) :: 'a','b','c'])
  if (len (z) /= 0 .or. size (z) /= 3) stop 3
  deallocate (z)
  allocate (z, source=[ character(0) :: cc ])
  if (len (z) /= 0 .or. size (z) /= 4) stop 4
  deallocate (z)
  associate (x => f())
    if (len (x) /= 0 .or. size (x) /= 1) stop 5
    if (x(1) /= '') stop 6
  end associate
contains
  function f() result(z)
    character(:), allocatable :: z(:)
    allocate (z, source=[''])
  end function f
end
