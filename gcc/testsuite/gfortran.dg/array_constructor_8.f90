! Like array_constructor_6.f90, but check constructors that mix iterators
! and individual scalar elements.
! { dg-do run }
program main
  implicit none
  call build (42)
contains
  subroutine build (order)
    integer :: order, i

    call test (order, 8, 5, (/ ((/ 1, 2, 3, 4, 5, 6, 7, 8 /), i = 1, order), &
                               100, 200, 300, 400, 500 /))

    call test (order, 2, 3, (/ ((/ 1, 2 /), i = 1, order), &
                               100, 200, 300 /))

    call test (order, 3, 5, (/ ((/ 1, 2, 3 /), i = 1, order), &
                               100, 200, 300, 400, 500 /))

    call test (order, 6, 1, (/ ((/ 1, 2, 3, 4, 5, 6 /), i = 1, order), &
                               100 /))

    call test (order, 5, 0, (/ ((/ 1, 2, 3, 4, 5 /), i = 1, order) /))

    call test (order, 0, 4, (/ 100, 200, 300, 400 /))

    call test (11, 5, 2, (/ ((/ 1, 2, 3, 4, 5 /), i = 1, 11), &
                            100, 200 /))

    call test (6, 2, order, (/ ((/ 1, 2 /), i = 1, 6), &
                               (i * 100, i = 1, order) /))
  end subroutine build

  subroutine test (order, repeat, trail, values)
    integer, dimension (:) :: values
    integer :: order, repeat, trail, i

    if (size (values, dim = 1) .ne. order * repeat + trail) STOP 1
    do i = 1, order * repeat
      if (values (i) .ne. mod (i - 1, repeat) + 1) STOP 2
    end do
    do i = 1, trail
      if (values (i + order * repeat) .ne. i * 100) STOP 3
    end do
  end subroutine test
end program main
