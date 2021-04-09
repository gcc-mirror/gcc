! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
!
! PR fortran/92587
!

module m
  implicit none (type, external)
  type t2
  contains
    final :: fini
  end type
  type t3
    type(t2) :: a
  end type
  type, extends(t3) :: t4
  end type
  class(t4), allocatable :: y
  class(t4), allocatable :: z
  integer :: fini_cnt = 0
contains
  subroutine sub
    y = z
  end
  subroutine fini(x)
    type(t2) :: x
    fini_cnt = fini_cnt + 1
  end
end

module m2
  use m
  implicit none (type, external)
  type, extends(t3) :: t5
  end type
  type, extends(t3) :: t6
  contains
    final :: fin2
  end type
  integer :: fin2_cnt = 0
contains
  subroutine bar(x, y, z)
    class(t4), allocatable, intent(out) :: x
    class(t5), allocatable, intent(out) :: y
    class(t6), allocatable, intent(out) :: z
  end
  subroutine fin2 (x)
    type(t6) :: x
    fin2_cnt = fin2_cnt + 1
  end
end  

  use m
  use m2
  implicit none (type, external)
  class(t4), allocatable :: x2
  class(t5), allocatable :: y2
  class(t6), allocatable :: z2

  if (fini_cnt /= 0 .or. fin2_cnt /= 0) stop 1
  call bar (x2, y2, z2)
  if (fini_cnt /= 0 .or. fin2_cnt /= 0) stop 2
  if (allocated(x2) .or. allocated(y2) .or. allocated(z2)) stop 3

  allocate(t4 :: x2)
  allocate(t5 :: y2)
  allocate(t6 :: z2)
  call bar (x2, y2, z2)
  if (fini_cnt /= 3 .or. fin2_cnt /= 1) stop 4
  if (allocated(x2) .or. allocated(y2) .or. allocated(z2)) stop 5

  allocate(t6 :: z2)
  call bar (x2, y2, z2)
  if (fini_cnt /= 4 .or. fin2_cnt /= 2) stop 6
  if (allocated(x2) .or. allocated(y2) .or. allocated(z2)) stop 7
end

! { dg-final { scan-tree-dump "__final_m_T2 \\\(struct" "original" } }
! { dg-final { scan-tree-dump "__final_m_T3 \\\(struct" "original" } }
! { dg-final { scan-tree-dump "__final_m2_T6 \\\(struct" "original" } }
