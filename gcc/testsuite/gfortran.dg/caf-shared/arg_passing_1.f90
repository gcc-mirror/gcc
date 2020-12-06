! { dg-do run }
! { dg-set-target-env-var GFORTRAN_NUM_IMAGES "4" }
! Miscellaneous tests for argument passing.
program main
  implicit none
  integer, dimension(4):: j[*]
  integer:: i[*]
  i = this_image()
  j = [3, 4, 5, 6] + 100*this_image()

  sync all

  call bar(i)
  call baz(j)
  call baw(j)
  call bay(j)
contains
  subroutine bar(x)
    integer, intent(in):: x[*]
    integer :: yy
    yy = x
    if (yy .ne. this_image()) stop 1
  end subroutine

  subroutine baz(x)
    integer, intent(in), dimension(:):: x[*]
    if (x(1) -100*this_image() .ne. 3) stop 2
    if (x(4) -100*this_image() .ne. 6) stop 3
    if (any(x(:)[1] - 100 .ne. [3, 4, 5, 6])) stop 4
  end subroutine

  subroutine baw(x)
    integer, parameter :: large = 10**7
    integer, intent(in), dimension(large:):: x[*]
    integer:: y, y2
    ! print *, this_image(), "baw: x, lbound x", x, lbound(x)
    y = x(large)
    y2 = x(large + 3) 
    if (y -100*this_image() .ne. 3) stop 5
    if (y2 -100*this_image() .ne. 6) stop 6
  end subroutine

  subroutine bay(x)
    integer, intent(in), dimension(:):: x
    if (x(1) - 100*this_image() .ne. 3) stop 7
    if (x(4) - 100*this_image() .ne. 6) stop 8
  end subroutine

  subroutine baa(x)
    integer, intent(in):: x
    if (x .ne. this_image()) stop 9
  end subroutine
end program
