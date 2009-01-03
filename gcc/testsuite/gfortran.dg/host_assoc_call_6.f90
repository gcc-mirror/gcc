! { dg-do compile }
!
! PR fortran/38594, in which the symtree for the first
! 'g' was being attached to the second. This is necessary
! for generic interfaces(eg. hosts_call_3.f90) but makes
! a mess otherwise.
!
! Contributed by Daniel Franke <dfranke@gcc.gnu.org>
!
MODULE m
CONTAINS
  SUBROUTINE g()
  END SUBROUTINE
  SUBROUTINE f()
    CALL g()
  CONTAINS
    SUBROUTINE g()
    END SUBROUTINE
  END SUBROUTINE
END MODULE

  USE m
  CALL g()
END
! { dg-final { cleanup-modules "m" } }
