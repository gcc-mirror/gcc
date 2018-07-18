! { dg-do compile }
! { dg-options -std=gnu }
      Subroutine RestoreR8Run()
      Implicit NONE
      Integer   ISTORE
      Real      XSTORE
      character   CSTORE(8)
      data cstore/8*' '/
      data istore/0/
      EQUIVALENCE (CSTORE(1),XSTORE,ISTORE) ! { dg-error "Overlapping unequal" }
      end
