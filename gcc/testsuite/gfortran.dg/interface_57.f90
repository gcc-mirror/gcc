! { dg-do compile }
! { dg-options "-Wexternal-argument-mismatch" }
! PR 119157 - this used to ICE because undo state was not
! correctly handled.

MODULE lmdif_module
  implicit none
   CONTAINS
      SUBROUTINE lmdif (fcn, m)
        EXTERNAL fcn
        integer m
        call fcn (m)
      END SUBROUTINE lmdif
END MODULE
