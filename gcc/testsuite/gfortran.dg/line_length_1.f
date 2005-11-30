! Testcase for -ffixed-line-length-none
! { dg-do compile }
! { dg-options "-ffixed-line-length-none" }
      program one
       if (abs(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa).gt.999.d0.or.abs(bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb).gt.999.d0.or.abs(cccccccccccccccccccc).gt.999.d0) THEN
       endif
      end program one
