! { dg-do run }
! PR 48066 - this used to segfault.
program p
  real(8) :: empty(0, 3), square(0)
  logical :: lempty(0, 3), lsquare(0)
  square = sum(empty * empty, 2)
  lsquare = any(lempty .and. lempty, 2)
end
