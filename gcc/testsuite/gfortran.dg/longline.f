# 1 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.f"
! { dg-do compile }
! { dg-options "-std=legacy" }

      subroutine foo
      character*10 cpnam
      character*4 csig
      write (34,808)  csig,ilax,cpnam
  808 format (/9X,4HTHE ,A4,  29HTIVE MINOS ERROR OF PARAMETER,I3,   2H
     +, ,A10)
      end
