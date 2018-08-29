! { dg-do run }
! PR22327
program array_constructor
    implicit none
    integer :: a(6), i
    i = 6
    a = (/ 1, 2, 3, 4, 5, i /)
    do i = 1, 6
        if (a(i) /= i) STOP 1
    end do
end program array_constructor
