! { dg-do compile }
! Option passed to avoid excess errors from obsolete warning
! { dg-options "-w" }
! PR18827
      integer i,j
      common /foo/ i,j
      assign 1000 to j
      j = 5
      goto j
 1000 continue
      end
