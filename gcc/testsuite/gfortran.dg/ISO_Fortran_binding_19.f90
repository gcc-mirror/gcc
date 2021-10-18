! { dg-do run }
! This testcase failed before with optimization as
! allocatef's CFI descriptor argument 'x' failed with -fstrict-alias due to
! internally alising with the GFC descriptor
!

program testit
  use iso_c_binding
  implicit none (external, type)
  type, bind (c) :: m
    integer(C_INT) :: i, j
  end type
  type(m), allocatable :: a(:)

  call testf (a)

contains
  subroutine allocatef (x) bind (c)
    type(m), allocatable :: x(:)
    allocate (x(5:15))
  end subroutine

  subroutine testf (y)
    type(m), allocatable, target :: y(:)
    call allocatef (y)
    if (.not. allocated (y))  stop 1
  end subroutine
end program
