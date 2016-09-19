! { dg-do compile }
! { dg-options "-fcoarray=lib -fdump-tree-original" }
!
  implicit none
  type t
  end type t
  class(t), allocatable :: y(:)[:]
  call bar()
  call foo(y)
contains
  subroutine bar(x)
    class(t), optional :: x(:)[*]
  end subroutine bar
  subroutine foo(x)
    class(t) :: x(:)[*]
  end subroutine foo
end
! { dg-final { scan-tree-dump-times "foo \\(struct __class_MAIN___T_1_1t & restrict x, void \\* restrict caf_token.., integer\\(kind=\[48\]\\) caf_offset..\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "bar \\(struct __class_MAIN___T_1_1t \\* x, void \\* restrict caf_token.., integer\\(kind=\[48\]\\) caf_offset..\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "bar \\(0B, 0B, 0\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "foo \\(&class.., y._data.token, \\(integer\\(kind=\[48\]\\)\\) class..._data.data - \\(integer\\(kind=\[48\]\\)\\) y._data.data\\);" 1 "original" } }
