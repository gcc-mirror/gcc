! { dg-do run }
! PR fortran/85976
! Original code from Stephan Kramer <stephan.kramer at imperial dot ac.uk>
program foo

  implicit none

  call bar(2, 3, 5, 7)

  contains

  subroutine bar(k, l, m, n)

    integer, intent(in) :: k, l, m, n
    real :: a(k), b(k,l), c(k,l,m), d(k,l,m,n)

    if (size(spread(A, 1, 1)) /= k) stop 1
    if (size(spread(b, 1, 1)) /= k * l) stop 2
    if (size(spread(c, 1, 1)) /= k * l * m) stop 3
    if (size(spread(d, 1, 1)) /= k * l * m * n) stop 4

  end subroutine

end program
