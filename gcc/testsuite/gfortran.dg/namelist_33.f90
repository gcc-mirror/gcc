! { dg-do compile }
!
! PR fortran/32876 - accepts private items in public NAMELISTs
!
! USE-associated types with private components may
! not be used in namelists -- anywhere.
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

  namelist /a/ t1          ! { dg-error "use-associated PRIVATE components" }
  namelist /b/ t4          ! { dg-error "use-associated PRIVATE components" }

  integer, private :: i
  namelist /c/ i           ! { dg-error "was declared PRIVATE and cannot be member of PUBLIC namelist" }

contains
  subroutine y()
   type(tp2) :: y2
   type(tp3) :: y3

    namelist /nml2/ y2     ! { dg-error "has use-associated PRIVATE components " }
    namelist /nml3/ y3     ! { dg-error "has use-associated PRIVATE components " }
  end subroutine
END MODULE


program xxx
  use types

  type :: tp5
    TYPE(tp4) :: t        ! nested private components
  end type
  type(tp5) :: t5

  namelist /nml/ t5       ! { dg-error "has use-associated PRIVATE components" }

contains
  subroutine z()
    namelist /nml2/ t5    ! { dg-error "has use-associated PRIVATE components" }
  end subroutine
end program
