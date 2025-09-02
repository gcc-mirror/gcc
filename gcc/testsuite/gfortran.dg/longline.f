# 1 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.f"
! { dg-do compile }
! { dg-options "-std=legacy" }

      subroutine foo
      character*10 cpnam
      character*4 csig
      write (34,808)  csig,ilax,cpnam
  808 format (/9X,'THE ',A4,     'TIVE MINOS ERROR OF PARAMETER',I3,   '
     +,' ,A10)
      end
