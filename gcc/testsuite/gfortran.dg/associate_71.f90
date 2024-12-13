! { dg-do run }
!
! Check that pr117347 is fixed.
! Contributed by Ivan Pribec  <ivan.pribec@gmail.com>

program pr117347
  implicit none

  type :: point
     real :: x = 42.
  end type point

  type(point) :: mypoint
  real        :: pi(1)
  associate (points =>  mypoint )
    pi(:) = points% x
  end associate
  if (any(pi /= 42)) stop 1
  associate (points => (mypoint))
    pi(:) = points% x
  end associate
  if (any(pi /= 42)) stop 2
  associate (points => [mypoint])
    pi(:) = points% x
  end associate
  if (any(pi /= 42)) stop 3
  associate (points => [rpoint()])
    pi(:) = points% x
  end associate
  if (any(pi /= 35)) stop 4

contains

  function rpoint() result(r)
    type(point) :: r
    r%x = 35
  end function
end program

