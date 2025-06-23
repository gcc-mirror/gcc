! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

subroutine f0(x)
  implicit none
  integer, value :: x
  !$acc wait(x) if(.false.) async
end

subroutine f1(y, ia)
  implicit none
  integer, value :: y, ia
  !$acc wait(y) if(.true.) async(ia)
end

subroutine fl(z, ll)
  implicit none
  integer, value :: z
  logical, value :: ll
  !$acc wait(z) if(ll) async(3)
end

subroutine a0(a)
  implicit none
  integer, value :: a
  !$acc wait(a) if(.false.)
end

subroutine a1(b)
  implicit none
  integer, value :: b
  !$acc wait(b) if(.true.)
end

subroutine al(c, qq)
  implicit none
  integer, value :: c
  logical, value :: qq
  !$acc wait(c) if(qq)
end

! { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = x;\[\\n\\r\]+ *if \\(0\\)\[\\n\\r\]+ *\{\[\\n\\r\]+ *__builtin_GOACC_wait \\(-1, 1, D\.\[0-9\]+\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = ia;\[\\n\\r\]+ *D\.\[0-9\]+ = y;\[\\n\\r\]+ *if \\(1\\)\[\\n\\r\]+ *\{\[\\n\\r\]+ *__builtin_GOACC_wait \\(D\.\[0-9\]+, 1, D\.\[0-9\]+\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = z;\[\\n\\r\]+ *D\.\[0-9\]+ = ll;\[\\n\\r\]+ *if \\(D\.\[0-9\]+\\)\[\\n\\r\]+ *\{\[\\n\\r\]+ *__builtin_GOACC_wait \\(3, 1, D\.\[0-9\]+\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = a;\[\\n\\r\]+ *if \\(0\\)\[\\n\\r\]+ *\{\[\\n\\r\]+ *__builtin_GOACC_wait \\(-2, 1, D\.\[0-9\]+\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = b;\[\\n\\r\]+ *if \\(1\\)\[\\n\\r\]+ *\{\[\\n\\r\]+ *__builtin_GOACC_wait \\(-2, 1, D\.\[0-9\]+\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "D\.\[0-9\]+ = c;\[\\n\\r\]+ *D\.\[0-9\]+ = qq;\[\\n\\r\]+ *if \\(D\.\[0-9\]+\\)\[\\n\\r\]+ *\{\[\\n\\r\]+ *__builtin_GOACC_wait \\(-2, 1, D\.\[0-9\]+\\);" 1 "original" } }
