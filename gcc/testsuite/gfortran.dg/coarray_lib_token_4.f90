! { dg-do compile }
! { dg-options "-fcoarray=lib -fdump-tree-original" }
!
! Check argument passing with assumed-shape coarray dummies
!
program test_caf
  implicit none
  integer, allocatable :: A(:)[:]
  integer, save :: B(3)[*]
  integer :: i

  allocate (A(3)[*])
  A = [1, 2, 3 ] 
  B = [9, 7, 4 ]
  call foo (A, A, test=1)
  call foo (A(2:3), B, test=2)
  call foo (B, A, test=3)
contains
  subroutine foo(x, y, test)
    integer :: x(:)[*]
    integer, contiguous :: y(:)[*]
    integer :: test
    call bar (x)
    call expl (y)
  end subroutine foo

  subroutine bar(y)
    integer :: y(:)[*]
  end subroutine bar

  subroutine expl(z)
    integer :: z(*)[*]
  end subroutine expl
end program test_caf

! { dg-final { scan-tree-dump-times "expl \\(integer\\(kind=4\\).0:. . restrict z, void . restrict caf_token.\[0-9\]+, integer\\(kind=.\\) caf_offset.\[0-9\]+\\)" 1 "original" } }
!
! { dg-final { scan-tree-dump-times "bar \\(struct array1_integer\\(kind=4\\) & restrict y, void . restrict caf_token.\[0-9\]+, integer\\(kind=.\\) caf_offset.\[0-9\]+\\)" 1 "original" } }
!
! { dg-final { scan-tree-dump-times "foo \\(struct array1_integer\\(kind=4\\) & restrict x, struct array1_integer\\(kind=4\\) & restrict y, integer\\(kind=4\\) & restrict test, void . restrict caf_token.\[0-9\]+, integer\\(kind=.\\) caf_offset.\[0-9\]+, void . restrict caf_token.\[0-9\]+, integer\\(kind=.\\) caf_offset.\[0-9\]+\\)" 1 "original" } }
!
! { dg-final { scan-tree-dump-times "bar \\(&parm.\[0-9\]+, caf_token.\[0-9\]+, \\(\\(integer\\(kind=.\\)\\) parm.\[0-9\]+.data - \\(integer\\(kind=.\\)\\) x.\[0-9\]+\\) \\+ caf_offset.\[0-9\]+\\);" 1 "original" } }
!
! { dg-final { scan-tree-dump-times "expl \\(\\(integer\\(kind=4\\).0:. .\\) parm.\[0-9\]+.data, caf_token.\[0-9\]+, \\(\\(integer\\(kind=.\\)\\) parm.\[0-9\]+.data - \\(\\(integer\\(kind=.\\)\\) y.\[0-9\]+\\) \\+ caf_offset.\[0-9\]+\\);" 0 "original" } }
!
! { dg-final { scan-tree-dump-times "foo \\(&a, &a, &C.\[0-9\]+, a.token, 0, a.token, 0\\);" 1 "original" } }
!
! { dg-final { scan-tree-dump-times "foo \\(&parm.\[0-9\]+, &parm.\[0-9\]+, &C.\[0-9\]+, a.token, \\(integer\\(kind=.\\)\\) parm.\[0-9\]+.data - \\(integer\\(kind=.\\)\\) a.data, caf_token.\[0-9\]+, \\(integer\\(kind=.\\)\\) parm.\[0-9\]+.data - \\(integer\\(kind=.\\)\\) b\\);" 1 "original" } }
!
! { dg-final { scan-tree-dump-times "foo \\(&parm.\[0-9\]+, &a, &C.\[0-9\]+, caf_token.\[0-9\]+, \\(integer\\(kind=.\\)\\) parm.\[0-9\]+.data - \\(integer\\(kind=.\\)\\) b, a.token, 0\\);" 1 "original" } }
!
