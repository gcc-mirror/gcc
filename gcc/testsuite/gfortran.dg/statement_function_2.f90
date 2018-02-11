! { dg-do compile }
! PR fortran/54223
subroutine r(d)
    implicit none
    integer, optional :: d
    integer :: h, q
    q(d) = d + 1     ! statement function statement
    h = q(d)
end subroutine r

subroutine s(x)
    implicit none
    integer, optional :: x
    integer :: g, z
    g(x) = x + 1     ! statement function statement
    z = g()          ! { dg-error "Missing actual argument" }
end subroutine s

subroutine t(a)
    implicit none
    integer :: a
    integer :: f, y
    f(a) = a + 1     ! statement function statement
    y = f()          ! { dg-error "Missing actual argument" }
end subroutine t
! { dg-prune-output " Obsolescent feature" }
