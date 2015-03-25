! { dg-do compile }
!
! PR fortran/65532
! The partial initialization through data statements was producing
! shape mismatch errors.
!
! Contributed by Harald Anlauf  <anlauf@gmx.de>

module gfcbug131
  implicit none
contains
  DOUBLE PRECISION FUNCTION d1mach(i)
    INTEGER, INTENT(IN)         :: i

    INTEGER :: small(4)
    INTEGER :: large(4)
    INTEGER :: right(4)
    INTEGER :: diver(4)
    INTEGER :: LOG10(4)
    DOUBLE PRECISION :: dmach(5)

    EQUIVALENCE (dmach(1),small(1))
    EQUIVALENCE (dmach(2),large(1))
    EQUIVALENCE (dmach(3),right(1))
    EQUIVALENCE (dmach(4),diver(1))
    EQUIVALENCE (dmach(5),LOG10(1))

    DATA small(1),small(2) /          0,    1048576 /
    DATA large(1),large(2) /         -1, 2146435071 /
    DATA right(1),right(2) /          0, 1017118720 /
    DATA diver(1),diver(2) /          0, 1018167296 /
    DATA LOG10(1),LOG10(2) / 1352628735, 1070810131 /

    d1mach = dmach(i)
  END FUNCTION d1mach

  DOUBLE PRECISION FUNCTION foo (x)
    DOUBLE PRECISION, INTENT(IN) :: x
    foo = SQRT (d1mach(4))
  END FUNCTION foo

end module gfcbug131

