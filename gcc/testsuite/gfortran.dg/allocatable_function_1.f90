! { dg-do run }
! { dg-options "-O2 -fdump-tree-original" }
! Test ALLOCATABLE functions; the primary purpose here is to check that
! each of the various types of reference result in the function result
! being deallocated, using _gfortran_internal_free.
! The companion, allocatable_function_1r.f90, executes this program.
!
subroutine moobar (a)
    integer, intent(in) :: a(:)

    if (.not.all(a == [ 1, 2, 3 ])) STOP 1
end subroutine moobar

function foo2 (n)
    integer, intent(in) :: n
    integer, allocatable :: foo2(:)
    integer :: i
    allocate (foo2(n))
    do i = 1, n
        foo2(i) = i
    end do
end function foo2

module m
contains
    function foo3 (n)
        integer, intent(in) :: n
        integer, allocatable :: foo3(:)
        integer :: i
        allocate (foo3(n))
        do i = 1, n
            foo3(i) = i
        end do
    end function foo3
end module m

program alloc_fun

    use m
    implicit none

    integer :: a(3)

    interface
      subroutine moobar (a)
          integer, intent(in) :: a(:)
      end subroutine moobar
    end interface

    interface
        function foo2 (n)
            integer, intent(in) :: n
            integer, allocatable :: foo2(:)
        end function foo2
    end interface

! 2 _gfortran_internal_free's
    if (.not.all(foo1(3) == [ 1, 2, 3 ])) STOP 2
    a = foo1(size(a))

! 1 _gfortran_internal_free
    if (.not.all(a == [ 1, 2, 3 ])) STOP 3
    call foobar(foo1(3))

! 1 _gfortran_internal_free
    if (.not.all(2*bar(size(a)) + 5 == [ 7, 9, 11 ])) STOP 4

! Although the rhs determines the loop size, the lhs reference is
! evaluated, in case it has side-effects or is needed for bounds checking.
! 3 _gfortran_internal_free's
    a(1:size (bar (3))) = 2*bar(size(a)) + 2 + a(size (bar (3)))
    if (.not.all(a == [ 7, 9, 11 ])) STOP 5

! 3 _gfortran_internal_free's
    call moobar(foo1(3))   ! internal function
    call moobar(foo2(3))   ! module function
    call moobar(foo3(3))   ! explicit interface

! 9 _gfortran_internal_free's in total
contains

    subroutine foobar (a)
        integer, intent(in) :: a(:)

        if (.not.all(a == [ 1, 2, 3 ])) STOP 6
    end subroutine foobar

    function foo1 (n)
        integer, intent(in) :: n
        integer, allocatable :: foo1(:)
        integer :: i
        allocate (foo1(n))
        do i = 1, n
            foo1(i) = i
        end do
    end function foo1

    function bar (n) result(b)
        integer, intent(in) :: n
        integer, target, allocatable :: b(:)
        integer :: i

        allocate (b(n))
        do i = 1, n
            b(i) = i
        end do
    end function bar

end program alloc_fun
! { dg-final { scan-tree-dump-times "__builtin_free " 10 "original" } }
