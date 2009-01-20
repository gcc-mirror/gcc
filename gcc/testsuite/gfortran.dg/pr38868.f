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
      LINE(78:80)='xyz'
      WRITE(*,'(A82)') "'"//LINE//"'"
      END

! { dg-final { scan-rtl-dump-times "line\\\+80" 0 "expand" } }
! { dg-final { cleanup-rtl-dump "expand" } } */
