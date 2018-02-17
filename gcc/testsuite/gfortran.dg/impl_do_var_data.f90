! { dg-do run }
! PR 80442
! This test case used to produce an bogus error
! about the variables being below the lower
! array bounds
program main
    implicit none
    integer:: i
    integer, dimension(3):: A
    data (A(i:i+2:i+1), i=1,2) /1, 2, 3/
    if(any(A .ne. [1,3,2])) STOP 1
end program
