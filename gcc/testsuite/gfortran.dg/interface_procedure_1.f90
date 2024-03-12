! { dg-do compile }
! { dg-additional-options "-std=f95" }
!
! PR fortran/48776
! The following used to generate a segmentation fault in the front-end,
! because a pointer to the get1 symbol was remaining in the get interface
! after the procedure statement was rejected and the symbol freed.

  interface get
    procedure get1  ! { dg-error "Fortran 2003: PROCEDURE statement" }
  end interface

  integer :: h
  call set1 (get (h))  ! { dg-error "no specific function for the generic 'get'" }
contains
  subroutine set1 (a)
    integer, intent(in) :: a
  end subroutine

  integer function get1 (s)
    integer :: s
  end function
end
