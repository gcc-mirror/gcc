! { dg-do compile }
! { dg-options "-fcoarray=lib -fdump-tree-original" }
!
! Check whether TOKEN and OFFSET are correctly propagated
! 

program main
  implicit none
  type t
    integer(4) :: a, b
  end type t
  integer :: caf[*]
  type(t) :: caf_dt[*]

  caf = 42
  caf_dt = t (1,2)
  call sub (caf, caf_dt%b)
  print *,caf, caf_dt%b
  if (caf /= -99 .or. caf_dt%b /= -101) call abort ()
  call sub_opt ()
  call sub_opt (caf)
  if (caf /= 124) call abort ()
contains

  subroutine sub (x1, x2)
    integer :: x1[*], x2[*]

    call sub2 (x1, x2)
  end subroutine sub

  subroutine sub2 (y1, y2)
    integer :: y1[*], y2[*]

    print *, y1, y2
    if (y1 /= 42 .or. y2 /= 2) call abort ()
    y1 = -99
    y2 = -101
  end subroutine sub2

  subroutine sub_opt (z)
    integer, optional :: z[*]
    if (present (z)) then
      if (z /= -99) call abort ()
      z = 124
    end if
  end subroutine sub_opt

end program main

! SCAN TREE DUMP AND CLEANUP
!
! PROTOTYPE 1:
!
! sub (integer(kind=4) * restrict x1, integer(kind=4) * restrict x2,
!      void * restrict caf_token.4, integer(kind=8) caf_offset.5,
!      void * restrict caf_token.6, integer(kind=8) caf_offset.7)
!
! { dg-final { scan-tree-dump-times "sub \\(integer.kind=4. . restrict x1, integer.kind=4. . restrict x2, void . restrict caf_token.\[0-9\]+, integer.kind=.. caf_offset.\[0-9\]+, void . restrict caf_token.\[0-9\]+, integer.kind=.. caf_offset.\[0-9\]+\\)" 1 "original" } }
!
! PROTOTYPE 2:
!
! sub2 (integer(kind=4) * restrict y1, integer(kind=4) * restrict y2,
!       void * restrict caf_token.0, integer(kind=8) caf_offset.1,
!       void * restrict caf_token.2, integer(kind=8) caf_offset.3)
!
! { dg-final { scan-tree-dump-times "sub2 \\(integer.kind=4. . restrict y1, integer.kind=4. . restrict y2, void . restrict caf_token.\[0-9\]+, integer.kind=.. caf_offset.\[0-9\]+, void . restrict caf_token.\[0-9\]+, integer.kind=.. caf_offset.\[0-9\]+\\)" 1 "original" } }
!
! CALL 1
!
!  sub ((integer(kind=4) *) caf, &caf_dt->b, caf_token.9, 0, caf_token.10, 4);
!
! { dg-final { scan-tree-dump-times "sub \\(\[^,\]*caf, &caf_dt->b, caf_token.\[0-9\]+, 0, caf_token.\[0-9\]+, 4\\)" 1 "original" } }
!
!  sub2 ((integer(kind=4) *) x1, (integer(kind=4) *) x2,
!        caf_token.4, NON_LVALUE_EXPR <caf_offset.5>,
!        caf_token.6, NON_LVALUE_EXPR <caf_offset.7>);
!
! { dg-final { scan-tree-dump-times "sub2 \\(\[^,\]*x1, \[^,\]*x2, caf_token.\[0-9]+, \[^,\]*caf_offset\[^,\]*, caf_token.\[0-9\]+, \[^,\]*caf_offset\[^,\]*\\)" 1 "original" } }
!
! CALL 3
!
! { dg-final { scan-tree-dump-times "sub_opt \\(0B, 0B, 0\\)" 1 "original" } }
!
! CALL 4
!
! { dg-final { scan-tree-dump-times "sub_opt \\(.integer.kind=4. .. caf, caf_token.\[0-9\]+, 0\\)" 1 "original" } }
!
! { dg-final { cleanup-tree-dump "original" } }
