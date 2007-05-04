! { dg-do compile }
! PR fortran/25071
! Check if actual argument is too short
!
        program test
           implicit none
           character(len=10) :: v
           character(len=10), target :: x
           character(len=20), target :: y
           character(len=30), target :: z
           character(len=10), pointer :: ptr1
           character(len=20), pointer :: ptr2
           character(len=30), pointer :: ptr3
           character(len=10), allocatable :: alloc1(:)
           character(len=20), allocatable :: alloc2(:)
           character(len=30), allocatable :: alloc3(:)
           call foo(v) ! { dg-error "actual argument shorter than of dummy" }
           call foo(x) ! { dg-error "actual argument shorter than of dummy" }
           call foo(y)
           call foo(z)
           ptr1 => x
           call foo(ptr1) ! { dg-error "actual argument shorter than of dummy" }
           call bar(ptr1) ! { dg-error "actual argument shorter than of dummy" }
           ptr2 => y
           call foo(ptr2)
           call bar(ptr2)
           ptr3 => z
           call foo(ptr3)
           call bar(ptr3) ! { dg-error "Character length mismatch" }
           allocate(alloc1(1))
           allocate(alloc2(1))
           allocate(alloc3(1))
           call arr(alloc1) ! { dg-error "actual argument shorter than of dummy" }
           call arr(alloc2)
           call arr(alloc3) ! { dg-error "Character length mismatch" }
        contains
        subroutine foo(y)
           character(len=20) :: y
           y = 'hello world'
        end subroutine
        subroutine bar(y)
           character(len=20),pointer :: y
           y = 'hello world'
        end subroutine
        subroutine arr(y)
           character(len=20),allocatable :: y(:)
           y(1) = 'hello world'
        end subroutine
       end
