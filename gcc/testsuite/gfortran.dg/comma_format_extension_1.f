! { dg-do compile }
! { dg-options "-std=legacy" }
! test that the extension for a missing comma is accepted

      subroutine mysub
      dimension ibar(5)
      write (3,1001) ( ibar(m), m = 1, 5 )

 1001 format (/5x,' ',i4' '/ )
      return
      end
