! PR fortran/69281
! { dg-do compile }
! { dg-additional-options "-fstack-arrays -O2" }

program pr69281
  implicit none
  call foo1((/ 1, 3, 3, 7 /))
  call foo2((/ 1, 3, 3, 7 /))
  call foo3((/ 1, 3, 3, 7 /))
  call foo4((/ 1, 3, 3, 7 /))
  call foo5((/ 1, 3, 3, 7 /))
  call foo6((/ 1, 3, 3, 7 /))
contains
  subroutine foo1(x)
    integer, intent(in) :: x(:)
    !$omp parallel
      call baz(bar(x))
    !$omp end parallel
  end subroutine
  subroutine foo2(x)
    integer, intent(in) :: x(:)
    !$omp task
      call baz(bar(x))
    !$omp end task
  end subroutine
  subroutine foo3(x)
    integer, intent(in) :: x(:)
    !$omp target
      call baz(bar(x))
    !$omp end target
  end subroutine
  subroutine foo4(x)
    integer, intent(in) :: x(:)
    !$omp target teams
      call baz(bar(x))
    !$omp end target teams
  end subroutine
  subroutine foo5(x)
    integer, intent(in) :: x(:)
    integer :: i
    !$omp parallel do
      do i = 1, 1
        call baz(bar(x))
      end do
  end subroutine
  subroutine foo6(x)
    integer, intent(in) :: x(:)
    integer :: i
    !$omp target teams distribute parallel do
      do i = 1, 1
        call baz(bar(x))
      end do
  end subroutine
  function bar(x) result(a)
    integer, dimension(:), intent(in) :: x
    integer, dimension(2,size(x)) :: a
    a(1,:) = 1
    a(2,:) = x
  end function
  subroutine baz(a)
    integer, dimension(:,:), intent(in) :: a
  end subroutine
end program
