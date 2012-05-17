! { dg-do compile }
!
! PR fortran/34763
! Before, gfortran did not allow for the "END" in
! the interface, which is no module procedure.
!
! Test case contributed by Dick Hendrickson
!
      module n
      contains
      subroutine n_interface
      INTERFACE
            SUBROUTINE NGSXDY(TLS1,TLS2)
            REAL  ::  TLS1,TLS2
            END ! OK
      END INTERFACE
      end subroutine
      end module
