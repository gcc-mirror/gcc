! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR 128316

module m
  implicit none
contains
  subroutine s (i)
    integer :: i
    if (i > 0) goto 20
    i = -1
  contains
    subroutine t
    end subroutine
20 end subroutine
end module

subroutine unref
contains
  subroutine u
  end subroutine
30 end subroutine

  use m
  implicit none
  type ty
  end type
  integer :: i
  i = 5
  call s (i)
  if (i /= 5) stop 1
  i = 0
  call s (i)
  if (i /= -1) stop 2
  call unref
  goto 17
  stop 3
contains
  subroutine sub
  end subroutine
17 end

! { dg-final { scan-tree-dump-times "__label_000017:" 1 "original" } }
! { dg-final { scan-tree-dump-times "__label_000020:" 1 "original" } }
! { dg-final { scan-tree-dump-times "__label_000030:" 1 "original" } }
