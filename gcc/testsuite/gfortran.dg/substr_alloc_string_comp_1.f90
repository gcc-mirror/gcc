! { dg-do run }
! PR fortran/65766
! Substrings of allocatable string components of derived types
program substr_derived_alloc_comp
    implicit none

    type t1
        character(len=:), allocatable :: s
    end type t1

    character(len=*), parameter :: c = &
      "0123456789abcdefghijklmnopqrstuvwxyz"

    type(t1) x1

    integer i

    x1%s = c

    do i = 1, 36
        if (x1%s(i:) .ne. c(i:)) call abort
    end do
end program
