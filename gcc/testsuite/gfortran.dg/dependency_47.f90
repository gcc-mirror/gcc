! { dg-do compile }
! Make sure there is only one instance of a temporary variable here.
! { dg-options "-fdump-tree-original" }

SUBROUTINE prtdata(ilen)
  INTEGER :: ilen
  character(len=ilen), allocatable :: cline(:)
  allocate(cline(2))
  cline(1) = 'a'
  cline(1)(2:3) = cline(1)(1:2)
  cline(2) = cline(1)
  print *,c
END SUBROUTINE prtdata
! { dg-final { scan-tree-dump-not "__var_2" "original" } }
! { dg-final { scan-tree-dump-times "__var_1" 3 "original" } }
