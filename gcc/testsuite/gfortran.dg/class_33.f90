! { dg-do compile }
!
! PR 46971: [4.6 Regression] [OOP] ICE on long class names
!
! Contributed by Andrew Benson <abenson@its.caltech.edu>

module Molecular_Abundances_Structure
  type molecularAbundancesStructure
  end type
  class(molecularAbundancesStructure), pointer :: molecules
end module

! { dg-final { cleanup-modules "Molecular_Abundances_Structure" } }
