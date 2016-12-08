! { dg-do compile }
! { dg-options "-O2 -fdump-rtl-expand" }
      PROGRAM testcase
      IMPLICIT NONE

      CHARACTER*4 ANER(18)
      CHARACTER*80 LINE
      aner = ''
      ANER(1)='A   '
      ANER(2)='    '
      LINE=' '
      LINE(78:80)='xyz'   ! { dg-warning "writing 3 bytes into a region of size 2" }
      WRITE(*,'(A82)') "'"//LINE//"'"
      END

! { dg-final { scan-rtl-dump-times "line\\\+80" 0 "expand" } }
