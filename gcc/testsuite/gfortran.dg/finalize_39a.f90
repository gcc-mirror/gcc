module m
  implicit none
  private
  
  type, public :: MyForm
    integer :: nValues = 0, nValuesUnpacked = 0, nVariables = 0
    integer, dimension ( : ), allocatable :: iaUnpacked
    real, dimension ( :, : ), allocatable :: Value
  contains
    procedure, private, pass :: InitializeEmpty
    procedure, private, pass :: InitializeCopy
    generic :: Initialize => InitializeEmpty, InitializeCopy
    final :: Finalize
  end type MyForm

  type, public :: MyElementForm
    class ( MyForm ), allocatable :: Element
  contains
    final :: FinalizeElement
  end type MyElementForm

contains

  subroutine InitializeEmpty ( PS, iaUnpacked, nValuesUnpacked, nVariables, ClearOption )
    class ( MyForm ), intent ( inout ) :: PS
    integer, dimension ( : ), intent ( in ) :: iaUnpacked
    integer, intent ( in ) :: nValuesUnpacked, nVariables
    logical, intent ( in ), optional :: ClearOption

    logical :: ClearRequested

    ClearRequested = .false.
    if ( present ( ClearOption ) ) &
      ClearRequested = ClearOption

    PS % nVariables = nVariables
    PS % nValues = size ( iaUnpacked )
    PS % nValuesUnpacked = nValuesUnpacked

    allocate ( PS % iaUnpacked ( PS % nValues ) )
    allocate ( PS % Value ( PS % nValues, PS % nVariables ) )
    if ( ClearRequested ) &
      call Clear ( PS % Value )
  end subroutine InitializeEmpty

  subroutine InitializeCopy ( PS, PS_Source )
    class ( MyForm ), intent ( inout ) :: PS
    class ( MyForm ), intent ( in ) :: PS_Source

    PS % nVariables = PS_Source % nVariables
    PS % nValues = PS_Source % nValues
    PS % nValuesUnpacked = PS_Source % nValuesUnpacked

    allocate ( PS % iaUnpacked ( PS % nValues ) )
    allocate ( PS % Value ( PS % nValues, PS % nVariables ) )
  end subroutine InitializeCopy

  elemental subroutine Finalize ( PS )
    type ( MyForm ), intent ( inout ) :: PS

    if ( allocated ( PS % Value ) ) &
      deallocate ( PS % Value )
    if ( allocated ( PS % iaUnpacked ) ) &
      deallocate ( PS % iaUnpacked )
  end subroutine Finalize

  subroutine LoadVariable ( PackedValue, UnpackedValue, iaUnpacked, nValues )
    real, dimension ( : ), intent ( inout ) :: PackedValue
    real, dimension ( : ), intent ( in ) :: UnpackedValue
    integer, dimension ( : ), intent ( in ) :: iaUnpacked
    integer, intent ( in ) :: nValues

    integer :: iV

    do iV = 1, nValues
      PackedValue ( iV ) = UnpackedValue ( iaUnpacked ( iV ) )
    end do
  end subroutine LoadVariable

  subroutine AddVariable ( PackedValue, UnpackedValue, iaUnpacked, nValues )
    real, dimension ( : ), intent ( inout ) :: PackedValue
    real, dimension ( : ), intent ( in ) :: UnpackedValue
    integer, dimension ( : ), intent ( in ) :: iaUnpacked
    integer, intent ( in ) :: nValues

    integer :: iV

    do iV = 1, nValues
      PackedValue ( iV ) = PackedValue ( iV ) + UnpackedValue ( iaUnpacked ( iV ) )
    end do
  end subroutine AddVariable

  subroutine StoreVariable ( UnpackedValue, PackedValue, iaUnpacked, nValues )
    real, dimension ( : ), intent ( inout ) :: UnpackedValue
    real, dimension ( : ), intent ( in ) :: PackedValue
    integer, dimension ( : ), intent ( in ) :: iaUnpacked
    integer, intent ( in ) :: nValues
    
    integer :: iV
    
    do iV = 1, nValues
      UnpackedValue ( iaUnpacked ( iV ) ) = PackedValue ( iV )
    end do
  end subroutine StoreVariable
  

  impure elemental subroutine FinalizeElement ( PSE )
    type ( MyElementForm ), intent ( inout ) :: PSE
    if ( allocated ( PSE % Element ) ) &
      deallocate ( PSE % Element )
  end subroutine FinalizeElement
end module m
