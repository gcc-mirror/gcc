! { dg-do compile }
! Option passed to avoid excess errors from obsolete warning
! { dg-options "-w" }
! PR18827
      integer i,j
      equivalence (i,j)
      assign 1000 to i
      write (*, j) ! { dg-error "not been assigned a format label" }
      goto j   ! { dg-error "not been assigned a target label" }
 1000 continue
      end
