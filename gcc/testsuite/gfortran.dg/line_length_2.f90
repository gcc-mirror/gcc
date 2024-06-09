! Testcase for -ffree-line-length-none
! See PR fortran/21302
! { dg-do compile }
! { dg-options "-ffree-line-length-none -std=f2018" }
program two
 if (abs(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa).gt.999.d0.or.abs(bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb).gt.999.d0.or.abs(cccccccccccccccccccc).gt.999.d0) THEN
 endif
end program two
