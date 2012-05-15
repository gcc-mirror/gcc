! { dg-do run }
! Test the fix for PR42736, in which an excessively rigorous dependency
! checking for the assignment generated an unnecessary temporary, whose
! rank was wrong.  When accessed by the scalarizer, a segfault ensued.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
! Reported by Armelius Cameron <armeliusc@gmail.com>
!
module UnitValue_Module

  implicit none
  private

  public :: &
    operator(*), &
    assignment(=)

  type, public :: UnitValue
    real :: &
      Value = 1.0
    character(31) :: &
      Label
  end type UnitValue

  interface operator(*)
    module procedure ProductReal_LV
  end interface operator(*)

  interface assignment(=)
    module procedure Assign_LV_Real
  end interface assignment(=)

contains

  elemental function ProductReal_LV(Multiplier, Multiplicand) result(P_R_LV)

    real, intent(in) :: &
      Multiplier
    type(UnitValue), intent(in) :: &
      Multiplicand
    type(UnitValue) :: &
      P_R_LV

    P_R_LV%Value = Multiplier * Multiplicand%Value
    P_R_LV%Label = Multiplicand%Label

  end function ProductReal_LV


  elemental subroutine Assign_LV_Real(LeftHandSide, RightHandSide)

    real, intent(inout) :: &
      LeftHandSide
    type(UnitValue), intent(in) :: &
      RightHandSide

    LeftHandSide = RightHandSide%Value

  end subroutine Assign_LV_Real

end module UnitValue_Module

program TestProgram

  use UnitValue_Module

  implicit none

  type :: TableForm
    real, dimension(:,:), allocatable :: &
      RealData
  end type TableForm

  type(UnitValue) :: &
    CENTIMETER

  type(TableForm), pointer :: &
    Table

  allocate(Table)
  allocate(Table%RealData(10,5))

  CENTIMETER%value = 42
  Table%RealData = 1
  Table%RealData(:,1) = Table%RealData(:,1) * CENTIMETER
  Table%RealData(:,2) = Table%RealData(:,2) * CENTIMETER
  Table%RealData(:,3) = Table%RealData(:,3) * CENTIMETER
  Table%RealData(:,5) = Table%RealData(:,5) * CENTIMETER

!  print *, Table%RealData
  if (any (abs(Table%RealData(:,4) - 1) > epsilon(1.0))) call abort ()
  if (any (abs(Table%RealData(:,[1,2,3,5]) - 42) > epsilon(1.0))) call abort ()
end program TestProgram
