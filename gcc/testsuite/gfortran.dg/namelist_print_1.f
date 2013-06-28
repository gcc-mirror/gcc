! Test Non standard PRINT namelist - PR21432
!
! Contributor Paul Thomas  <pault@gcc.gnu.org>
!
! { dg-do run }
! { dg-options "-std=gnu" }

      real  x
      namelist /mynml/ x
      x = 1
! { dg-output "^" }
      print mynml ! { dg-output "&MYNML(\n|\r\n|\r) X=  1.00000000    ,(\n|\r\n|\r) /(\n|\r\n|\r)" }
      end
