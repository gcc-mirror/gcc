module testmod
  implicit none

  contains

  subroutine foo(n)
    integer, intent(in) :: n
    real :: r(0:n,-n:n), a(0:n,-n:n), dj
    integer :: k, j

    ! initialize with some dummy values
    do j = -n, n
      a(:, j) = j
      r(:,j) = j + 1
    end do

    ! here be dragons
    do k = 0, n
      dj = r(k, k - 2) * a(k, k - 2)
      r(k,k) = a(k, k - 1) * dj
    enddo

    if (r(0,0) .ne. -2.) call abort

  end subroutine

end module

program test
  use testmod
  implicit none
  call foo(5)
end program
