! { dg-do compile }
!
! PR fortran/57895
!
! Contributed by Vittorio Zecca
!
c Segmentation fault in gfc_restore_last_undo_checkpoint
      COMMON RADE3155V62$JUTMU9L9E(3,3,3), LADE314JUTMP9         ! { dg-error "Invalid character '\\$' at .1.. Use '-fdollar-ok' to allow it as an extension" }
     +LHEDDJNTMP9L(3,3,3)                                                       
      end
! { dg-excess-errors "compilation terminated" }
