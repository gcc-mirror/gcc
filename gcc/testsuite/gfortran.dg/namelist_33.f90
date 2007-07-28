! { dg-do compile }
!
! PR fortran/32876 - accepts private items in public NAMELISTs
!
MODULE types
  type :: tp4
    PRIVATE
    real :: x
    integer :: i
  end type

  ! nested type
  type :: tp3
    real :: x
    integer, private :: i
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
   type(tp4) :: t4

   namelist /a/ t1    ! { dg-error "has PRIVATE components and cannot be a member of PUBLIC namelist" }
   namelist /b/ t4    ! { dg-error "has PRIVATE components and cannot be a member of PUBLIC namelist" }

  integer, private :: i
  namelist /c/ i      ! { dg-error "was declared PRIVATE and cannot be member of PUBLIC namelist" }
END MODULE

! { dg-final { cleanup-modules "types nml" } }
