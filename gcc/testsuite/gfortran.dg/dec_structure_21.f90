      ! { dg-do compile }
      ! { dg-options "-ffixed-form" }
      !
      ! Test errors for %FILL without -fdec-structure.
      !
      implicit none

      character(2) %fill ! { dg-error "is a DEC extension" }

      end
