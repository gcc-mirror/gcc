! { dg-do compile }
!
! PR fortran/32905 - accepts types with ultimate POINTER components
!
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

   namelist /a/ t1    ! { dg-error "has ALLOCATABLE or POINTER components and thus requires a defined input/output" }
   namelist /b/ t3    ! { dg-error "has ALLOCATABLE or POINTER components and thus requires a defined input/output" }
END MODULE
