! PR 12840.  Make sure that array constructors can be used to determine
! the bounds of a scalarization loop.
! { dg-do run }
program main
  implicit none
  call build (11)
contains
  subroutine build (order)
    integer :: order, i

    call test (order, (/ (i * 2, i = 1, order) /))
    call test (17, (/ (i * 2, i = 1, 17) /))
    call test (5, (/ 2, 4, 6, 8, 10 /))
  end subroutine build

  subroutine test (order, values)
    integer, dimension (:) :: values
    integer :: order, i

    if (size (values, dim = 1) .ne. order) STOP 1
    do i = 1, order
      if (values (i) .ne. i * 2) STOP 2
    end do
  end subroutine test
end program main
