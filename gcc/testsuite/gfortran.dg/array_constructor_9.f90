! Like array_constructor_6.f90, but check constructors in which the length
! of each subarray can only be determined at run time.
! { dg-do run }
program main
  implicit none
  call build (9)
contains
  function gen (order)
    real, dimension (:, :), pointer :: gen
    integer :: order, i, j

    allocate (gen (order, order + 1))
    forall (i = 1 : order, j = 1 : order + 1) gen (i, j) = i * i + j
  end function gen

  ! Deliberately leaky!
  subroutine build (order)
    integer :: order, i

    call test (order, 0, (/ (gen (i), i = 1, order) /))
    call test (3, 2, (/ ((/ 1.5, 1.5, gen (i) /), i = 1, 3) /))
  end subroutine build

  subroutine test (order, prefix, values)
    real, dimension (:) :: values
    integer :: order, prefix, last, i, j, k

    last = 0
    do i = 1, order
      do j = 1, prefix
        last = last + 1
        if (values (last) .ne. 1.5) call abort
      end do
      do j = 1, i + 1
        do k = 1, i
          last = last + 1
          if (values (last) .ne. j + k * k) call abort
        end do
      end do
    end do
    if (size (values, dim = 1) .ne. last) call abort
  end subroutine test
end program main
