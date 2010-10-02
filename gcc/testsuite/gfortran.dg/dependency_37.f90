! { dg-do compile }
! { dg-options "-Warray-temporaries" }
! PR 48231 - this used to create an unnecessary temporary.
module UnitValue_Module
  type :: UnitValue
    real          :: Value = 1.0
  end type

  interface operator(*)
    module procedure ProductReal_LV
  end interface operator(*)

  interface assignment(=)
    module procedure Assign_LV_Real
  end interface assignment(=)
contains

  elemental function ProductReal_LV(Multiplier, Multiplicand) result(P_R_LV)
    real, intent(in)            :: Multiplier
    type(UnitValue), intent(in) :: Multiplicand
    type(UnitValue)             :: P_R_LV
    P_R_LV%Value = Multiplier * Multiplicand%Value
  end function ProductReal_LV

  elemental subroutine Assign_LV_Real(LeftHandSide, RightHandSide)
    real, intent(inout)         :: LeftHandSide
    type(UnitValue), intent(in) :: RightHandSide
    LeftHandSide = RightHandSide%Value
  end subroutine Assign_LV_Real
end module UnitValue_Module

program TestProgram
  use UnitValue_Module

  type :: TableForm
    real, dimension(:,:), allocatable :: RealData
  end type TableForm

  REAL :: CENTIMETER
  type(TableForm), pointer :: Table

  allocate(Table)
  allocate(Table%RealData(10,5))

  CENTIMETER = 42
  Table%RealData = 1
  Table%RealData(:,1) = Table%RealData(:,1) * CENTIMETER
end program TestProgram
! { dg-final { cleanup-modules "UnitValue_Module" } }
