! Like array_constructor_6.f90, but check iterators with non-default stride,
! including combinations which lead to zero-length vectors.
! { dg-do run }
! { dg-options "-Wzerotrip" }
program main
  implicit none
  call build (77)
contains
  subroutine build (order)
    integer :: order, i, j

    call test (1, 11, 3, (/ (i, i = 1, 11, 3) /))
    call test (3, 20, 2, (/ (i, i = 3, 20, 2) /))
    call test (4, 0, 11, (/ (i, i = 4, 0, 11) /)) ! { dg-warning "will be executed zero times" }

    call test (110, 10, -3,  (/ (i, i = 110, 10, -3) /))
    call test (200, 20, -12, (/ (i, i = 200, 20, -12) /))
    call test (29, 30, -6,   (/ (i, i = 29, 30, -6) /)) ! { dg-warning "will be executed zero times" }

    call test (1, order, 3,  (/ (i, i = 1, order, 3) /))
    call test (order, 1, -3, (/ (i, i = order, 1, -3) /))

    ! Triggers compile-time iterator calculations in trans-array.c
    call test (1, 1000, 2,   (/ (i, i = 1, 1000, 2),   (i, i = order, 0, 1) /))
    call test (1, 0, 3,      (/ (i, i = 1, 0, 3),      (i, i = order, 0, 1) /)) ! { dg-warning "will be executed zero times" }
    call test (1, 2000, -5,  (/ (i, i = 1, 2000, -5),  (i, i = order, 0, 1) /)) ! { dg-warning "will be executed zero times" }
    call test (3000, 99, 4,  (/ (i, i = 3000, 99, 4),  (i, i = order, 0, 1) /)) ! { dg-warning "will be executed zero times" }
    call test (400, 77, -39, (/ (i, i = 400, 77, -39), (i, i = order, 0, 1) /))

    do j = -10, 10
      call test (order + j, order, 5,  (/ (i, i = order + j, order, 5) /))
      call test (order + j, order, -5, (/ (i, i = order + j, order, -5) /))
    end do

  end subroutine build

  subroutine test (from, to, step, values)
    integer, dimension (:) :: values
    integer :: from, to, step, last, i

    last = 0
    do i = from, to, step
      last = last + 1
      if (values (last) .ne. i) call abort
    end do
    if (size (values, dim = 1) .ne. last) call abort
  end subroutine test
end program main
