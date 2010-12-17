! Test the DSHIFTL and DSHIFTR intrinsics.
!
! { dg-do run }
! { dg-options "-ffree-line-length-none" }
! { dg-require-effective-target fortran_integer_16 }

  implicit none

#define RESL(I,J,SHIFT) \
  IOR(SHIFTL(I,SHIFT),SHIFTR(J,BIT_SIZE(J)-SHIFT))
#define RESR(I,J,SHIFT) \
  IOR(SHIFTL(I,BIT_SIZE(I)-SHIFT),SHIFTR(J,SHIFT))

#define CHECK(I,J,SHIFT) \
  if (dshiftl(I,J,SHIFT) /= RESL(I,J,SHIFT)) call abort ; \
  if (dshiftr(I,J,SHIFT) /= RESR(I,J,SHIFT)) call abort ; \
  if (run_dshiftl(I,J,SHIFT) /= RESL(I,J,SHIFT)) call abort ; \
  if (run_dshiftr(I,J,SHIFT) /= RESR(I,J,SHIFT)) call abort

  CHECK(0_16,0_16,0)
  CHECK(0_16,0_16,1)
  CHECK(0_16,0_16,7)
  CHECK(0_16,0_16,8)
  CHECK(28_16,79_16,0)
  CHECK(28_16,79_16,1)
  CHECK(28_16,79_16,5)
  CHECK(28_16,79_16,7)
  CHECK(28_16,79_16,8)
  CHECK(-28_16,79_16,0)
  CHECK(-28_16,79_16,1)
  CHECK(-28_16,79_16,5)
  CHECK(-28_16,79_16,7)
  CHECK(-28_16,79_16,8)
  CHECK(28_16,-79_16,0)
  CHECK(28_16,-79_16,1)
  CHECK(28_16,-79_16,5)
  CHECK(28_16,-79_16,7)
  CHECK(28_16,-79_16,8)
  CHECK(-28_16,-79_16,0)
  CHECK(-28_16,-79_16,1)
  CHECK(-28_16,-79_16,5)
  CHECK(-28_16,-79_16,7)
  CHECK(-28_16,-79_16,8)

contains

  function run_dshiftl (i, j, shift) result(res)
    integer(kind=16) :: i, j, res
    integer :: shift
    res = dshiftl(i,j,shift)
  end function

  function run_dshiftr (i, j, shift) result(res)
    integer(kind=16) :: i, j, res
    integer :: shift
    res = dshiftr(i,j,shift)
  end function

end
