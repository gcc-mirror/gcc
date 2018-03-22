! Like array_constructor_6.f90, but check constructors that apply
! an elemental function to an array.
! { dg-do run }
program main
  implicit none
  call build (200)
contains
  subroutine build (order)
    integer :: order, i

    call test (order, (/ (abs ((/ i, -i, -i * 2 /)), i = 1, order) /))
    call test (order, abs ((/ ((/ -i, -i, i * 2 /), i = 1, order) /)))
    call test (order, (/ abs ((/ ((/ i, i, -i * 2 /), i = 1, order) /)) /))
  end subroutine build

  subroutine test (order, values)
    integer, dimension (3:) :: values
    integer :: order, i

    if (size (values, dim = 1) .ne. order * 3) STOP 1
    do i = 1, order
      if (values (i * 3) .ne. i) STOP 2
      if (values (i * 3 + 1) .ne. i) STOP 3
      if (values (i * 3 + 2) .ne. i * 2) STOP 4
    end do
  end subroutine test
end program main
