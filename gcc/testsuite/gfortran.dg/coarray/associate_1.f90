!{ dg-do run }

! Contributed by Neil Carlson  <neil.n.carlson@gmail.com>
! Check PR110033 is fixed.

program coarray_associate_1
  type t
    integer :: b = -1
    logical :: l = .FALSE.
  end type

  integer :: x[*] = 10
  class(t), allocatable :: c[:]

  associate (y => x)
    y = -1
    y[1] = 35
  end associate
  allocate(c[*])
  associate (f => c)
    f%b = 17
    f[1]%l = .TRUE.
  end associate

  if (this_image() == 1) then
    if (x /= 35) stop 1
    if (c%b /= 17) stop 2
    if (.NOT. c%l) stop 3
  else
    if (x /= -1) stop 4
    if (c%b /= 17) stop 5
    if (c%l) stop 6
  end if

end

