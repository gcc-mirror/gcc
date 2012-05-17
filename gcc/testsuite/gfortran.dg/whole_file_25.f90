! { dg-do compile }
! { dg-options "-fwhole-program" }
!
! PR fortran/45087
!

module ints
   INTERFACE
      SUBROUTINE NOZZLE()
      END SUBROUTINE NOZZLE
   END INTERFACE
end module ints

      SUBROUTINE NOZZLE()
      END SUBROUTINE NOZZLE
      program CORTESA 
      USE INTS
      CALL NOZZLE ()
      END program CORTESA
