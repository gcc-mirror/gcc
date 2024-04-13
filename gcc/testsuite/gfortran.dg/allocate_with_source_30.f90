! { dg-do run }
! { dg-additional-options "-std=f2008 -fcheck=bounds -g -fdump-tree-original" }
! { dg-output "At line 43 .*" }
! { dg-shouldfail "Unequal character lengths .3/2. in ALLOCATE with SOURCE= or MOLD=" }
!
! PR fortran/113793
!
! Test runtime checks of string length for ALLOCATE with SOURCE= or MOLD=

program p
  implicit none
  character(kind=1,len=2) :: c1 =   "xx"
  character(kind=1,len=8) :: c2 =   "yy"
  character(kind=4,len=6) :: c3 = 4_"ww"
  call sub1 (len (c2), c2)
  call sub4 (len (c3), c3)
  call test (len (c1) + 1, c1)
contains
  subroutine sub1 (n, s)
    integer,      intent(in) :: n
    character(*), intent(in) :: s
    character(len=8), allocatable :: f(:), g
    character(len=n), allocatable :: h(:), j
    ALLOCATE (f(7), source=s)
    ALLOCATE (g,    source=s)
    ALLOCATE (h(5), mold=s)
    ALLOCATE (j,    mold=s)
  end
  subroutine sub4 (n, s)
    integer,                 intent(in) :: n
    character(kind=4,len=*), intent(in) :: s
    character(kind=4,len=6), allocatable :: f(:), g
    character(kind=4,len=n), allocatable :: h(:), j
    ALLOCATE (f(3), source=s)
    ALLOCATE (g,    source=s)
    ALLOCATE (h(5), mold=s)
    ALLOCATE (j,    mold=s)
  end
  subroutine test (n, s)
    integer,      intent(in) :: n
    character(*), intent(in) :: s
    character(len=n), allocatable :: str
    ALLOCATE (str, source=s)
  end
end

! { dg-final { scan-tree-dump-times "__builtin_malloc .72.;" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_malloc .24.;" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_malloc .56.;" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_malloc .8.;" 1 "original" } }
! { dg-final { scan-tree-dump-times "ALLOCATE with SOURCE= or MOLD=" 9 "original" } }
