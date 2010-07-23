! { dg-do compile }
!
! Private types and types with private components
! are acceptable in local namelists.
!

MODULE nml
  type :: tp1
    integer :: i
  end type

  type :: tp2
    private
    integer :: i
  end type

  private :: tp1
contains
  subroutine x()
   type(tp1) :: t1
   type(tp2) :: t2

    namelist /nml1/ i        ! ok, private variable
    namelist /nml2/ t1       ! ok, private type
    namelist /nml3/ t2       ! ok, private components
  end subroutine
END MODULE

! { dg-final { cleanup-modules "nml" } }
