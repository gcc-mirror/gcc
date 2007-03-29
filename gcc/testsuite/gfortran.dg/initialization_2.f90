! { dg-do run }
! Vector subscripts, ranks and shapes of initialization expressions (PRs 29393,
! 29630 and 29679)
program test

    implicit none
    integer :: i, j
    integer, parameter :: a(4,4,4) = reshape([ (i,i=1,64) ], [4,4,4])
    integer, parameter :: v(4) = [4, 1, 3, 2]

    integer :: b1(3,3) = a(1:3, 2, 2:4)
    integer :: b2(1,3) = a(2:2, 4, [1,4,3])
    integer :: b2b(3) = a([1,4,3], 2, 4)
    integer :: b3(4) = a(1, v, 3)
    integer :: b4(3,3) = a(v([2,4,3]), 2, [2,3,4])

    if (any(b1 /= reshape([21,22,23, 37,38,39, 53,54,55], [3,3]))) call abort()
    if (any(b2 /= reshape([14, 62, 46], [1,3]))) call abort()
    if (any(b2b /= [53, 56, 55])) call abort()
    if (any(b3 /= [45, 33, 41, 37])) call abort()
    if (any(b4 /= reshape([21,22,23, 37,38,39, 53,54,55], [3,3]))) call abort()
end program test
