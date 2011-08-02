! { dg-do compile }
!
! PR fortran/34785, in which the character length of BA_T was not
! passed on to the array constructor argument of SEQ.
!
! Contributed by Dick Hendrickson <dick.hendrickson@gmail.com>
!
      MODULE o_TYPE_DEFS
        implicit none
        TYPE SEQ
          SEQUENCE
          CHARACTER(len = 9) ::  BA(2)
        END TYPE SEQ
        CHARACTER(len = 9)   ::  BA_T(2)
        CHARACTER(LEN = 9)   ::  CA_T(1,2)
      END MODULE o_TYPE_DEFS

      MODULE TESTS
        use o_type_defs
        implicit none
      CONTAINS
        SUBROUTINE OG0015(UDS0L)
          TYPE(SEQ)          UDS0L
          integer :: j1
          UDS0L = SEQ((/ (BA_T(J1),J1=1,2) /))
        END SUBROUTINE
      END MODULE TESTS

      use o_type_defs
      CONTAINS
        SUBROUTINE OG0015(UDS0L)
          TYPE(SEQ)          UDS0L
          UDS0L = SEQ(RESHAPE ( (/ ((CA_T(J1,J2), J1 = 1, 1), J2 = 1, 2)/),(/2/)))
        END SUBROUTINE
      END
! { dg-final { cleanup-modules "o_type_defs tests" } }
