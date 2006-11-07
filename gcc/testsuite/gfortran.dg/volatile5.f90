! { dg-do compile }
! { dg-options "-O3 -fdump-tree-optimized" }
! Tests whether volatile really works with modules
! PR fortran/29601
module volmod
  implicit none
  integer, volatile :: a
  logical :: b,c
  volatile :: b
contains
  subroutine sample
    a = 33.
    if(a /= 432) print *,'aPresent'

    b = .false.
    if(b) print *,'bPresent'

    c = .false.
    if(c) print *,'cPresent'
  end subroutine sample
end module volmod

program main
  use volmod
  implicit none

  a = 432
  if(a /= 432) print *,'aStillPresent'

  b = .false.
  if(b)        print *,'bStillPresent'

  c = .false.
  if(c)        print *,'cStillPresent'
end program main
! { dg-final { scan-tree-dump "aPresent" "optimized" } }
! { dg-final { scan-tree-dump "bPresent" "optimized" } }
! { dg-final { scan-tree-dump "aStillPresent" "optimized" } }
! { dg-final { scan-tree-dump "bStillPresent" "optimized" } }
! { dg-final { scan-tree-dump-not "cPresent" "optimized" } }
! { dg-final { scan-tree-dump-not "cStillPresent" "optimized" } }
! { dg-final { cleanup-tree-dump "optimized" } }
! { dg-final { cleanup-modules "volmod" } }
