! Like array_alloc_1.f90, but check cases in which the array length is
! not a literal constant.
! { dg-do run }
program main
  implicit none
  integer, parameter :: n = 100
  call test (n, f1 ())
  call test (47, f2 (50))
  call test (n, f3 (f1 ()))
contains
  subroutine test (expected, x)
    integer, dimension (:) :: x
    integer :: i, expected
    if (size (x, 1) .ne. expected) STOP 1
    do i = 1, expected
      if (x (i) .ne. i * 100) STOP 2
    end do
  end subroutine test

  function f1 ()
    integer, dimension (n) :: f1
    integer :: i
    forall (i = 1:n) f1 (i) = i * 100
  end function f1

  function f2 (howmuch)
    integer :: i, howmuch
    integer, dimension (4:howmuch) :: f2
    forall (i = 4:howmuch) f2 (i) = i * 100 - 300
  end function f2

  function f3 (x)
    integer, dimension (:) :: x
    integer, dimension (size (x, 1)) :: f3
    integer :: i
    forall (i = 1:size(x)) f3 (i) = i * 100
  end function f3
end program main
