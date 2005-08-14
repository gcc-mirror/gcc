! Test Non standard PRINT namelist - PR21432 is
! not accepted by -std=f95
!
! Contributor Paul Thomas  <pault@gcc.gnu.org>
!
! { dg-do compile }
! { dg-options "-std=f95" }
!
      real  x
      namelist /mynml/ x
      x = 1
      print mynml  ! { dg-error "PRINT namelist.*extension" "" }
      end
