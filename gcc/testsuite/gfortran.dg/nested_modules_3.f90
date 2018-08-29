! { dg-do run }
!
! This tests the improved version of the patch for PR16861.  Testing
! after committing the first version, revealed that this test did
! not work but was not regtested for, either.
!
! Contributed by Paul Thomas <pault@gcc.gnu.org>
!
MODULE foo
  TYPE type1
    INTEGER i1
  END TYPE type1
END MODULE

MODULE bar
CONTAINS
  SUBROUTINE sub1 (x, y)
    USE foo
    TYPE (type1)  :: x
    INTEGER  :: y(x%i1)
    y = 1
  END SUBROUTINE SUB1
  SUBROUTINE sub2 (u, v)
    USE foo
    TYPE (type1)  :: u
    INTEGER  :: v(u%i1)
    v = 2
  END SUBROUTINE SUB2
END MODULE

MODULE foobar
  USE foo
  USE bar
CONTAINS
  SUBROUTINE sub3 (s, t)
    USE foo
    TYPE (type1)  :: s
    INTEGER  :: t(s%i1)
    t = 3
  END SUBROUTINE SUB3
END MODULE foobar

PROGRAM use_foobar
  USE foo
  USE foobar
  INTEGER :: j(3) = 0
  TYPE (type1)   :: z
  z%i1 = 3
  CALL sub1 (z, j)
  z%i1 = 2
  CALL sub2 (z, j)
  z%i1 = 1
  CALL sub3 (z, j)
  IF (ALL (j.ne.(/3,2,1/))) STOP 1
END PROGRAM use_foobar
