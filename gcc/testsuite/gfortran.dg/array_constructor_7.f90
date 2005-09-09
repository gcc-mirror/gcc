! Like array_constructor_6.f90, but test for nested iterators.
! { dg-do run }
program main
  implicit none
  call build (17)
contains
  subroutine build (order)
    integer :: order, i, j

    call test (order, (/ (((j + 100) * i, j = 1, i), i = 1, order) /))
    call test (9, (/ (((j + 100) * i, j = 1, i), i = 1, 9) /))
    call test (3, (/ 101, 202, 204, 303, 306, 309 /))
  end subroutine build

  subroutine test (order, values)
    integer, dimension (:) :: values
    integer :: order, i, j

    if (size (values, dim = 1) .ne. order * (order + 1) / 2) call abort
    do i = 1, order
      do j = 1, i
        if (values (i * (i - 1) / 2 + j) .ne. (j + 100) * i) call abort
      end do
    end do
  end subroutine test
end program main
