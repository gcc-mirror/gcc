! { dg-do compile }
!
! PR fortran/122046
! The check for illegal recursion used to trigger on assertion when resolving
! the array spec of the dummy argument in the submodule
!
! Contributed by Tomáš Trnka <trnka@scm.com>

module ChemicalSystemModule

   implicit none
   private

   type, public :: ChemicalSystemType
   contains
      procedure, public  :: NumAtoms
   end type

contains

   elemental integer function NumAtoms(self)
      class(ChemicalSystemType), intent(in) :: self

      NumAtoms = 123

   end function

end module

module ChemicalSystemUtilsModule

   use ChemicalSystemModule

   implicit none
   private

   public :: ChemicalSystemRMSD

   interface

      module subroutine ChemicalSystemRMSD(modelSys, rmsdGrad)
         type(ChemicalSystemType), intent(in)  :: modelSys
         real                    , intent(out) :: rmsdGrad(3,modelSys%NumAtoms())
      end subroutine

   end interface

end module

submodule(ChemicalSystemUtilsModule) ChemicalSystemUtilsSubModule
   use ChemicalSystemModule

   implicit none

contains

   module subroutine ChemicalSystemRMSD(modelSys, rmsdGrad)
      type(ChemicalSystemType), intent(in)   :: modelSys
      real                    , intent(out)  :: rmsdGrad(3,modelSys%NumAtoms())
   end subroutine

end submodule

