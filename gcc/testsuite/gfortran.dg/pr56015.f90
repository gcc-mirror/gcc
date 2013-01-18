! PR middle-end/56015
! { dg-do run }
! { dg-options "-Ofast -fno-inline" }

program pr56015
  implicit none
  complex*16 p(10)
  p(:) = (0.1d0, 0.2d0)
  p(:) = (0.0d0, 1.0d0) * p(:)
  call foo (p)
contains
  subroutine foo (p)
    complex*16 p(10)
    if (any (p .ne. (-0.2d0, 0.1d0))) call abort
  end subroutine
end program pr56015
