! { dg-do compile }
! PR 34305 - Test for specifying a real as dimension
      program test
      real , parameter :: dsize = 1000
      dimension idata (dsize) ! { dg-error "scalar INTEGER expression" }
      idata (1) = -1    ! { dg-error "must have the pointer attribute" }
      end
