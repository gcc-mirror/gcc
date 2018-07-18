! { dg-do compile }
! { dg-options -std=f95 }
! PR fortran/32905 - accepts types with ultimate POINTER components
! updated for PR78659
MODULE types
  type :: tp3
    real :: x
    integer, pointer :: i
  end type

  type :: tp2
    type(tp3) :: t
  end type

  type :: tp1
    integer :: i
    type(tp2) :: t
  end type
END MODULE

MODULE nml
USE types
   type(tp1) :: t1
   type(tp3) :: t3
! The following are allowed under f2003.
   namelist /a/ t1    ! { dg-error "with ALLOCATABLE or POINTER components" }
   namelist /b/ t3    ! { dg-error "with ALLOCATABLE or POINTER components" }
END MODULE
