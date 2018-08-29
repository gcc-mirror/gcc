! Test the DSHIFTL and DSHIFTR intrinsics.
!
! { dg-do run }
! { dg-options "-ffree-line-length-none" }

  implicit none

  interface run_dshiftl
    procedure dshiftl_1
    procedure dshiftl_2
    procedure dshiftl_4
    procedure dshiftl_8
  end interface
  interface run_dshiftr
    procedure dshiftr_1
    procedure dshiftr_2
    procedure dshiftr_4
    procedure dshiftr_8
  end interface

#define RESL(I,J,SHIFT) \
  IOR(SHIFTL(I,SHIFT),SHIFTR(J,BIT_SIZE(J)-SHIFT))
#define RESR(I,J,SHIFT) \
  IOR(SHIFTL(I,BIT_SIZE(I)-SHIFT),SHIFTR(J,SHIFT))

#define CHECK(I,J,SHIFT) \
  if (dshiftl(I,J,SHIFT) /= RESL(I,J,SHIFT)) STOP 1; \
  if (dshiftr(I,J,SHIFT) /= RESR(I,J,SHIFT)) STOP 2; \
  if (run_dshiftl(I,J,SHIFT) /= RESL(I,J,SHIFT)) STOP 3; \
  if (run_dshiftr(I,J,SHIFT) /= RESR(I,J,SHIFT)) STOP 4

  CHECK(0_1,0_1,0)
  CHECK(0_1,0_1,1)
  CHECK(0_1,0_1,7)
  CHECK(0_1,0_1,8)
  CHECK(28_1,79_1,0)
  CHECK(28_1,79_1,1)
  CHECK(28_1,79_1,5)
  CHECK(28_1,79_1,7)
  CHECK(28_1,79_1,8)
  CHECK(-28_1,79_1,0)
  CHECK(-28_1,79_1,1)
  CHECK(-28_1,79_1,5)
  CHECK(-28_1,79_1,7)
  CHECK(-28_1,79_1,8)
  CHECK(28_1,-79_1,0)
  CHECK(28_1,-79_1,1)
  CHECK(28_1,-79_1,5)
  CHECK(28_1,-79_1,7)
  CHECK(28_1,-79_1,8)
  CHECK(-28_1,-79_1,0)
  CHECK(-28_1,-79_1,1)
  CHECK(-28_1,-79_1,5)
  CHECK(-28_1,-79_1,7)
  CHECK(-28_1,-79_1,8)

  CHECK(0_2,0_2,0)
  CHECK(0_2,0_2,1)
  CHECK(0_2,0_2,7)
  CHECK(0_2,0_2,8)
  CHECK(28_2,79_2,0)
  CHECK(28_2,79_2,1)
  CHECK(28_2,79_2,5)
  CHECK(28_2,79_2,7)
  CHECK(28_2,79_2,8)
  CHECK(-28_2,79_2,0)
  CHECK(-28_2,79_2,1)
  CHECK(-28_2,79_2,5)
  CHECK(-28_2,79_2,7)
  CHECK(-28_2,79_2,8)
  CHECK(28_2,-79_2,0)
  CHECK(28_2,-79_2,1)
  CHECK(28_2,-79_2,5)
  CHECK(28_2,-79_2,7)
  CHECK(28_2,-79_2,8)
  CHECK(-28_2,-79_2,0)
  CHECK(-28_2,-79_2,1)
  CHECK(-28_2,-79_2,5)
  CHECK(-28_2,-79_2,7)
  CHECK(-28_2,-79_2,8)

  CHECK(0_4,0_4,0)
  CHECK(0_4,0_4,1)
  CHECK(0_4,0_4,7)
  CHECK(0_4,0_4,8)
  CHECK(28_4,79_4,0)
  CHECK(28_4,79_4,1)
  CHECK(28_4,79_4,5)
  CHECK(28_4,79_4,7)
  CHECK(28_4,79_4,8)
  CHECK(-28_4,79_4,0)
  CHECK(-28_4,79_4,1)
  CHECK(-28_4,79_4,5)
  CHECK(-28_4,79_4,7)
  CHECK(-28_4,79_4,8)
  CHECK(28_4,-79_4,0)
  CHECK(28_4,-79_4,1)
  CHECK(28_4,-79_4,5)
  CHECK(28_4,-79_4,7)
  CHECK(28_4,-79_4,8)
  CHECK(-28_4,-79_4,0)
  CHECK(-28_4,-79_4,1)
  CHECK(-28_4,-79_4,5)
  CHECK(-28_4,-79_4,7)
  CHECK(-28_4,-79_4,8)

  CHECK(0_8,0_8,0)
  CHECK(0_8,0_8,1)
  CHECK(0_8,0_8,7)
  CHECK(0_8,0_8,8)
  CHECK(28_8,79_8,0)
  CHECK(28_8,79_8,1)
  CHECK(28_8,79_8,5)
  CHECK(28_8,79_8,7)
  CHECK(28_8,79_8,8)
  CHECK(-28_8,79_8,0)
  CHECK(-28_8,79_8,1)
  CHECK(-28_8,79_8,5)
  CHECK(-28_8,79_8,7)
  CHECK(-28_8,79_8,8)
  CHECK(28_8,-79_8,0)
  CHECK(28_8,-79_8,1)
  CHECK(28_8,-79_8,5)
  CHECK(28_8,-79_8,7)
  CHECK(28_8,-79_8,8)
  CHECK(-28_8,-79_8,0)
  CHECK(-28_8,-79_8,1)
  CHECK(-28_8,-79_8,5)
  CHECK(-28_8,-79_8,7)
  CHECK(-28_8,-79_8,8)


contains

  function dshiftl_1 (i, j, shift) result(res)
    integer(kind=1) :: i, j, res
    integer :: shift
    res = dshiftl(i,j,shift)
  end function
  function dshiftl_2 (i, j, shift) result(res)
    integer(kind=2) :: i, j, res
    integer :: shift
    res = dshiftl(i,j,shift)
  end function
  function dshiftl_4 (i, j, shift) result(res)
    integer(kind=4) :: i, j, res
    integer :: shift
    res = dshiftl(i,j,shift)
  end function
  function dshiftl_8 (i, j, shift) result(res)
    integer(kind=8) :: i, j, res
    integer :: shift
    res = dshiftl(i,j,shift)
  end function

  function dshiftr_1 (i, j, shift) result(res)
    integer(kind=1) :: i, j, res
    integer :: shift
    res = dshiftr(i,j,shift)
  end function
  function dshiftr_2 (i, j, shift) result(res)
    integer(kind=2) :: i, j, res
    integer :: shift
    res = dshiftr(i,j,shift)
  end function
  function dshiftr_4 (i, j, shift) result(res)
    integer(kind=4) :: i, j, res
    integer :: shift
    res = dshiftr(i,j,shift)
  end function
  function dshiftr_8 (i, j, shift) result(res)
    integer(kind=8) :: i, j, res
    integer :: shift
    res = dshiftr(i,j,shift)
  end function

end
