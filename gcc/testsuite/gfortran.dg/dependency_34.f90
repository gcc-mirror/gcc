! { dg-do compile }
! { dg-options "-Warray-temporaries" }
module foo
  implicit none
contains
  integer pure  function bar(i,j)
    integer, intent(in) :: i,j
    bar = 3 - i + 1 * abs(i) + j
  end function bar
end module foo

program main
  use foo
  implicit none
  real a(10)
  integer :: i
  read (*,*) a, i
  a(i:abs(i)) = a(i:abs(i))
  a(bar(i,i+2):2) = a(bar(i,i+2):2)
  a(int(i,kind=2):5) = a(int(i,kind=2)+1:6)
end program main
