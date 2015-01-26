! { dg-do run }
! { dg-options "-Warray-temporaries" }
! PR 57023
! This used to cause wrong packing because a(1:n,1:n) was
! assumed to be a full array.
module mymod
  implicit none
contains
  subroutine foo1(a,n)
    integer, dimension(n,n), intent(inout) :: a
    integer :: n
    n = n - 1
    call baz(a(1:n,1:n),n)  ! { dg-warning "array temporary" }
  end subroutine foo1

  subroutine foo2(a,n)
    integer, dimension(n,n), intent(inout) :: a
    integer :: n
    call decrement(n)
    call baz(a(1:n,1:n),n)  ! { dg-warning "array temporary" }
  end subroutine foo2

  subroutine foo3(a,n)
    integer, dimension(n,n), intent(inout) :: a
    integer :: n, m
    m = n - 1
    call baz(a(1:m,1:m),m)  ! { dg-warning "array temporary" }
  end subroutine foo3

  subroutine foo4(a,n)
    integer, dimension(n,n), intent(inout) :: a
    integer, intent(in) :: n
    a(1:n,1:n) = 1
  end subroutine foo4

  subroutine baz(a,n)
    integer, dimension(n,n), intent(inout) :: a
    integer, intent(in) :: n
    a = 1
  end subroutine baz

  subroutine decrement(n)
    integer, intent(inout) :: n
    n = n - 1
  end subroutine decrement

end module mymod

program main
  use mymod
  implicit none
  integer, dimension(5,5) :: a, b
  integer :: n

  b = 0
  b(1:4,1:4) = 1

  n = 5
  a = 0
  call foo1(a,n)
  if (any(a /= b)) call abort

  n = 5
  a = 0
  call foo2(a,n)
  if (any(a /= b)) call abort

  n = 5
  a = 0
  call foo3(a,n)
  if (any(a /= b)) call abort

  n = 5
  a = 0
  call foo4(a,n)
  if (any(a /= 1)) call abort
end program main
