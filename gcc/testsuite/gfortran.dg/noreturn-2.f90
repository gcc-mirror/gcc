! { dg-do compile }
! { dg-options "-O2 -Wuninitialized" }

subroutine foo1
implicit none
interface
subroutine bar1
!GCC$ ATTRIBUTES noreturn :: bar1
end subroutine
end interface
real,allocatable :: d(:) ! { dg-note "declared here" "note" }
d = 0. ! { dg-warning "used uninitialized" "uninitialized descriptor" }
call bar1()
d = 0. ! { dg-bogus "warning:" "not optimized out" }
end subroutine foo1

function foo2()
integer :: foo2
interface
subroutine bar2
!GCC$ ATTRIBUTES noreturn :: bar2
end subroutine
end interface
call bar2
return ! { dg-bogus "__result_foo2' is used uninitialized" "return" }
foo2 = 0
end function foo2

subroutine foo3
implicit none
integer :: i,j
interface
subroutine abort2
!GCC$ ATTRIBUTES noreturn :: abort2
end subroutine
end interface
call abort2()
do i=1,j-1 ; end do ! { dg-bogus "is used uninitialized" "uninitialized" }
end subroutine foo3

function foo4()
integer :: foo4
!$GCC$ ATTRIBUTES noreturn :: foo4
foo4 = 1
end function

subroutine foo5(k)
implicit none
integer :: i, k
!GCC$ ATTRIBUTES noreturn :: mpi_abort
call mpi_abort()
k = i
end subroutine
