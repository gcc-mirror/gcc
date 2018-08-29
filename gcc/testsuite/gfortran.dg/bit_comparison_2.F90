! Test the BGE, BGT, BLE and BLT intrinsics.
!
! { dg-do run }
! { dg-options "-ffree-line-length-none" }
! { dg-require-effective-target fortran_integer_16 }

#define CHECK(I,J,RES) \
  if (bge(I,J) .neqv. RES) STOP 1; \
  if (run_bge(I,J) .neqv. RES) STOP 2; \
  if (bgt(I,J) .neqv. (RES .and. (I/=J))) STOP 3; \
  if (run_bgt(I,J) .neqv. (RES .and. (I/=J))) STOP 4; \
  if (ble(J,I) .neqv. RES) STOP 5; \
  if (run_ble(J,I) .neqv. RES) STOP 6; \
  if (blt(J,I) .neqv. (RES .and. (I/=J))) STOP 7; \
  if (run_blt(J,I) .neqv. (RES .and. (I/=J))) STOP 8

#define T .true.
#define F .false.

  CHECK(0_16, 0_16, T)
  CHECK(1_16, 0_16, T)
  CHECK(0_16, 107_16, F)
  CHECK(5_16, huge(0_16) / 2_16, F)
  CHECK(5_16, huge(0_16), F)
  CHECK(-1_16, 0_16, T)
  CHECK(0_16, -19_16, F)
  CHECK(huge(0_16), -19_16, F)

contains

  pure logical function run_bge (i, j) result(res)
    integer(kind=16), intent(in) :: i, j
    res = bge(i,j)
  end function
  pure logical function run_bgt (i, j) result(res)
    integer(kind=16), intent(in) :: i, j
    res = bgt(i,j)
  end function
  pure logical function run_ble (i, j) result(res)
    integer(kind=16), intent(in) :: i, j
    res = ble(i,j)
  end function
  pure logical function run_blt (i, j) result(res)
    integer(kind=16), intent(in) :: i, j
    res = blt(i,j)
  end function

end
