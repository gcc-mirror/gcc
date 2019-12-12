! { dg-do run }
! { dg-additional-options "-O -fdump-tree-optimized" }
module y
  implicit none
contains
  subroutine foo(a,b,c,d,e,f)
    real, dimension(1), intent(inout) :: a, b, c, d, e, f
    if (any([a,b,c,d,e,f] /= [1,2,3,4,5,6])) stop 1
    a = -a
    b = -b
    c = -c
    d = -d
    e = -e
    f = -f
  end subroutine foo
end module y
module x
  use y
  implicit none
contains
  subroutine bar(a)
    real, dimension(:) :: a
    integer :: n1, n3, n5
    n1 = 1
    n3 = 3
    n5 = 5
    call foo(a(n1:n1), a(n1+1:n1+1), a(n3:n3), a(n3+1:n3+1), a(n5:n5), a(n5+1:n5+1))
  end subroutine bar
end module x

program main
  use x
  real, dimension(6) :: a,b
  b = [1,2,3,4,5,6]
  a = b
  call bar(a)
  if (any(a /= -b)) stop 2
end program main
! { dg-final { scan-tree-dump-not "contiguous" "optimized" } }
