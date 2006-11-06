! { dg-do compile }
! Check that bounds are checked when using vector subscripts in initialization
! expressions. (PR 29630)
program test

    implicit none
    integer :: i, j
    integer, parameter :: a(4,4,4) = reshape([ (i,i=1,64) ], [4,4,4])
    integer, parameter :: v(4) = [5, 1, -4, 2]

    integer :: b2(3) = a(2, 4, [1,7,3]) ! { dg-error "out of bounds" }
    integer :: b3(4) = a(1, v, 3) ! { dg-error "out of bounds" }
end program test
