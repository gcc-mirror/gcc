MODULE TEST
  IMPLICIT NONE
  INTEGER, PARAMETER :: dp=KIND(0.0D0)
  TYPE mulliken_restraint_type
    INTEGER                         :: ref_count
    REAL(KIND = dp)                 :: strength
    REAL(KIND = dp)                 :: TARGET
    INTEGER                         :: natoms
    INTEGER, POINTER, DIMENSION(:)  :: atoms
  END TYPE mulliken_restraint_type
CONTAINS
  SUBROUTINE INIT(mulliken)
   TYPE(mulliken_restraint_type), INTENT(INOUT) :: mulliken
   ALLOCATE(mulliken%atoms(1))
   mulliken%atoms(1)=1
   mulliken%natoms=1
   mulliken%target=0
   mulliken%strength=0
  END SUBROUTINE INIT
  SUBROUTINE restraint_functional(mulliken_restraint_control,charges, &
                                charges_deriv,energy,order_p)
    TYPE(mulliken_restraint_type), &
      INTENT(IN)                             :: mulliken_restraint_control
    REAL(KIND=dp), DIMENSION(:, :), POINTER  :: charges, charges_deriv
    REAL(KIND=dp), INTENT(OUT)               :: energy, order_p

    INTEGER                                  :: I
    REAL(KIND=dp)                            :: dum

    charges_deriv=0.0_dp
    order_p=0.0_dp

    DO I=1,mulliken_restraint_control%natoms
       order_p=order_p+charges(mulliken_restraint_control%atoms(I),1) &
                      -charges(mulliken_restraint_control%atoms(I),2)
    ENDDO
   
energy=mulliken_restraint_control%strength*(order_p-mulliken_restraint_control%target)**2
   
dum=2*mulliken_restraint_control%strength*(order_p-mulliken_restraint_control%target)
    DO I=1,mulliken_restraint_control%natoms
       charges_deriv(mulliken_restraint_control%atoms(I),1)=  dum
       charges_deriv(mulliken_restraint_control%atoms(I),2)= -dum
    ENDDO
END SUBROUTINE restraint_functional

END MODULE

    USE TEST
    IMPLICIT NONE
    TYPE(mulliken_restraint_type) :: mulliken
    REAL(KIND=dp), DIMENSION(:, :), POINTER  :: charges, charges_deriv
    REAL(KIND=dp) :: energy,order_p
    ALLOCATE(charges(1,2),charges_deriv(1,2))
    charges(1,1)=2.0_dp
    charges(1,2)=1.0_dp
    CALL INIT(mulliken)
    CALL restraint_functional(mulliken,charges,charges_deriv,energy,order_p)
    write(6,*) order_p
END

