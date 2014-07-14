! { dg-do run }
! Tests fix for PR61780 in which the loop reversal mechanism was
! not accounting for the first index being an element so that no
! loop in this dimension is created.
!
! Contributed by Manfred Tietze on clf.
!
program prgm3
    implicit none
    integer, parameter :: n = 10, k = 3
    integer :: i, j
    integer, dimension(n,n) :: y
    integer :: res1(n), res2(n)

1   format(10i5)

!initialize
    do i=1,n
        do j=1,n
            y(i,j) = n*i + j
        end do
    end do
    res2 = y(k,:)

!shift right
    y(k,4:n) = y(k,3:n-1)
    y(k,3) = 0
    res1 = y(k,:)
    y(k,:) = res2
    y(k,n:4:-1) = y(k,n-1:3:-1)
    y(k,3) = 0
    res2 = y(k,:)
!    print *, res1
!    print *, res2
    if (any(res1 /= res2)) call abort ()
end program prgm3
