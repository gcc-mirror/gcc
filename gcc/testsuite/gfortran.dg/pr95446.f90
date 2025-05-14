! { dg-do compile }
! { dg-options "-pedantic-errors" }
!
! Contributed by Martin Diehl  <m.diehl@mpie.de>

program elemental_optional
  implicit none
  integer :: m(5), r(5)

  m = 1

  r = outer()
  r = outer(m)
  
  contains

  function outer(o) result(l)
    integer, intent(in), optional :: o(:)
    integer :: u(5), l(5)

    l = inner(o,u)

  end function outer

  function outer_literal(o) result(l)
    integer, intent(in), optional :: o(5)
    integer :: l(5)

    l = inner(o, [1,2,3,4,5])
  end function outer_literal

  function outer_func(o) result(l)
    integer, intent(in), optional :: o(5)
    integer :: l(5)

    l = inner(o, outer())
  end function outer_func

  elemental function inner(a,b) result(x)
    integer, intent(in), optional :: a
    integer, intent(in) :: b
    integer :: x

    if(present(a)) then
      x = a*b
    else
      x = b
    endif
  end function inner
  
end program elemental_optional

