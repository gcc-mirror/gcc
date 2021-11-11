! { dg-do compile }
! { dg-options "-O0" }
! PR fortran/91424
! Check that only one warning is issued inside blocks, and that
! warnings are also issued for contained subroutines.

program main
  real :: a(5)
  block
    integer :: j
    do j=0, 5  ! { dg-warning "out of bounds" }
       a(j) = 2. ! { dg-warning "out of bounds" }
    end do
  end block
  call x
contains
  subroutine x
    integer :: i
    do i=1,6 ! { dg-warning "out of bounds" }
       a(i) = 2.  ! { dg-warning "out of bounds" }
    end do
  end subroutine x
end program main
