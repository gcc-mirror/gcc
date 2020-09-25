! { dg-do compile }
! { dg-options "-std=f2018" }
! PR fortran/97036 - [F2018] Allow ELEMENTAL RECURSIVE procedure prefix 

module m97036
  implicit none
contains
  impure elemental recursive subroutine foo (n)
    integer, intent(in) :: n
    integer :: k(n), f(n), i
    k = [ (i-1, i=1,n) ]
    f = fac (k)
    print *, f
  end subroutine foo
  elemental recursive subroutine bla ()
  end subroutine bla
  elemental recursive function fac (k) result (f)
    integer, intent(in) :: k
    integer             :: f
    f = 1
    if (k > 1) f = k*fac (k-1)
  end function fac
end module
  use m97036
  implicit none
  call foo ([4,5])
end
