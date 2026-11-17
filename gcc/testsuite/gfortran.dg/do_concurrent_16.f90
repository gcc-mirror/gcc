! { dg-do run }
!
! PR 128793

program main
  implicit none
  integer :: s, lo, hi, st, i, j, k, a(5), b(5,5)

  s = 2
  a = 0
  do concurrent (k = 1:5:s)
     a(k) = k
     if (k == 1) s = 1
  end do
  if (any (a /= [1, 0, 3, 0, 5])) stop 1

  lo = 1
  hi = 3
  st = 1
  b = 0
  do concurrent (i = lo:hi:st, j = 1:3)
     b(i,j) = 1
     if (i == 1 .and. j == 1) call change (lo, hi, st)
  end do
  if (any (b(1:3,1:3) /= 1) .or. sum (b) /= 9) stop 2

  lo = 1
  hi = 3
  st = 1
  b = 0
  do concurrent (j = 1:3, i = lo:hi:st)
     b(i,j) = 1
     if (i == 1 .and. j == 1) call change (lo, hi, st)
  end do
  if (any (b(1:3,1:3) /= 1) .or. sum (b) /= 9) stop 3

contains

  pure subroutine change (lo, hi, st)
    integer, intent(out) :: lo, hi, st
    lo = 2
    hi = 2
    st = 2
  end subroutine change

end program main
