! Compiled with pr83149.f90
! { dg-do run }
! { dg-compile-aux-modules "pr83149.f90" }
! { dg-additional-sources pr83149.f90 }
!
! Contributed by Neil Carlson  <neil.n.carlson@gmail.com>
!
subroutine sub(s)
  use mod2
  real :: s
  s = sum(get())
end

  use mod1
  real :: s
  ncells = 2
  call sub (s)
  if (int (s) .ne. ncells) stop 1
  ncells = 10
  call sub (s)
  if (int (s) .ne. ncells) stop 2
end

