! { dg-do run }
! PR 25806: Functions returning pointers to arrays
program a 
    integer, target :: storage(5)
    integer :: s(3)


    print *, x(3)  ! { dg-output " *1  *2  *3" }

    if (ssum(x(3)) /= 6) call abort()

    s = 0
    s = x(3)
    if (any(s /= (/1, 2, 3/))) call abort()

contains

    function x(n) result(t)
        integer, intent(in) :: n
        integer, pointer :: t(:)
        integer :: i

        t => storage(1:n)
        t = (/ (i, i = 1, n) /)

    end function x


    integer function ssum(a)
        integer, intent(in) :: a(:)

        ssum = sum(a)
        
    end function ssum

end program a


