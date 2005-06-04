subroutine gfc_debug_bug (n,m,k,ax,bx,c)
! above line must be the first line
! { dg-do compile }
! { dg-options "-g" }
! PR 19195
! we set line numbers wrongly, which made the compiler choke when emitting
! debug information.
  implicit none
  integer :: n, m
  integer :: k(n+m)
  real    :: ax(:), bx(n), c(n+m)

  integer :: i
  real    :: f

  i = k(n)
  f = c(n)
  f = bx(n)
  f = ax(n)
end subroutine gfc_debug_bug
