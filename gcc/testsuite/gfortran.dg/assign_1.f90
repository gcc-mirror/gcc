! { dg-do compile }
! Option passed to avoid excess errors from obsolete warning
! { dg-options "-w" }
      integer*4 i(5)
      assign 1000 to i   ! { dg-error "scalar default INTEGER" }
 1000 continue
      end
