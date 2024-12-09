! { dg-do run }
!
! Check that PR116669 is fixed now.
! Contributed by Dominik Gronkiewicz  <gronki@gmail.com>

program pr116669

    implicit none (type, external)

    type ast_expr_t
        type(ast_opcall_t), allocatable :: op_call
    end type

    type ast_opcall_t
        type(ast_expr_t), allocatable :: args(:)
    end type

    type(ast_opcall_t) :: o
   
    allocate(o%args(2))
    allocate(o%args(2)%op_call)
    allocate(o%args(2)%op_call%args(3))

    if (.NOT. allocated(o%args(2)%op_call%args)) stop 1

    deallocate(o%args)
end program

