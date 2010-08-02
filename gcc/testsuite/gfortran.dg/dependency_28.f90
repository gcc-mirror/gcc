! { dg-do compile }
! { dg-options "-Warray-temporaries" }
module foobar
  type baz
     integer :: i
     integer :: j
     integer :: k
     integer :: m
  end type baz
contains
  subroutine foo(a,b,c,i)
    real, dimension(10) :: a,b
    type(baz) :: c
    integer, dimension(10) :: i
    a(i(1):i(2)) = a(i(1):i(2)) + b(i(1):i(2))
    a(i(1):i(2)) = a(i(3):i(5)) ! { dg-warning "Creating array temporary" }
    a(c%i:c%j) = a(c%i:c%j) + b(c%k:c%m)
    a(c%k:c%m) = a(c%i:c%j) + b(c%k:c%m) ! { dg-warning "Creating array temporary" }
  end subroutine foo
end module foobar
! { dg-final { cleanup-modules "foobar" } }
