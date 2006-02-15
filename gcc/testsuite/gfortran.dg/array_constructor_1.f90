! { dg-do run }
! Check that [...] style array constructors work
program bracket_array_constructor
    implicit none
    integer :: a(4), i

    a = [ 1, 2, 3, 4 ]
    do i = 1, size(a)
        if (a(i) /= i) call abort()
    end do

    a = [ (/ 1, 2, 3, 4 /) ]
    do i = 1, size(a)
        if (a(i) /= i) call abort()
    end do

end program bracket_array_constructor
