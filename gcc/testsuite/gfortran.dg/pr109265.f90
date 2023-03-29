! PR tree-optimization/109265
! { dg-do compile }
! { dg-options "-O3 -w" }

module pr109265
  integer, parameter :: r8 = selected_real_kind (12)
contains
  subroutine foo (b, c, d, e, f)
    implicit none
    logical :: b
    real (kind = r8) :: c, d, e, f, i
    if (b) then
      c = bar (c * d, e)
      i = bar (f, c)
      call baz (i)
      call baz (-i)
    end if
  end subroutine foo
  function bar (a, b)
    implicit none
    real (kind = r8) :: bar
    real (kind = r8) :: a, b
    bar = a + b
  end function bar
  subroutine baz (b)
    implicit none
    real (kind = r8) :: b, d, e, f, g, h, i
    d = b
    i = 0
    e = d
    f = d
    g = d
  10 continue
    if ((e.eq.d) .and. (f.eq.d) .and. (g.eq.d) .and. (h.eq.d)) then
      h = i
      goto 10
    end if
  end subroutine baz
end module pr109265
