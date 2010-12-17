! Test the BGE, BGT, BLE and BLT intrinsics.
!
! { dg-do run }
! { dg-options "-ffree-line-length-none" }

  interface run_bge
    procedure run_bge1
    procedure run_bge2
    procedure run_bge4
    procedure run_bge8
  end interface

  interface run_bgt
    procedure run_bgt1
    procedure run_bgt2
    procedure run_bgt4
    procedure run_bgt8
  end interface

  interface run_ble
    procedure run_ble1
    procedure run_ble2
    procedure run_ble4
    procedure run_ble8
  end interface

  interface run_blt
    procedure run_blt1
    procedure run_blt2
    procedure run_blt4
    procedure run_blt8
  end interface

#define CHECK(I,J,RES) \
  if (bge(I,J) .neqv. RES) call abort ; \
  if (run_bge(I,J) .neqv. RES) call abort ; \
  if (bgt(I,J) .neqv. (RES .and. (I/=J))) call abort ; \
  if (run_bgt(I,J) .neqv. (RES .and. (I/=J))) call abort ; \
  if (ble(J,I) .neqv. RES) call abort ; \
  if (run_ble(J,I) .neqv. RES) call abort ; \
  if (blt(J,I) .neqv. (RES .and. (I/=J))) call abort ; \
  if (run_blt(J,I) .neqv. (RES .and. (I/=J))) call abort

#define T .true.
#define F .false.

  CHECK(0_1, 0_1, T)
  CHECK(1_1, 0_1, T)
  CHECK(0_1, 107_1, F)
  CHECK(5_1, huge(0_1) / 2_1, F)
  CHECK(5_1, huge(0_1), F)
  CHECK(-1_1, 0_1, T)
  CHECK(0_1, -19_1, F)
  CHECK(huge(0_1), -19_1, F)

  CHECK(0_2, 0_2, T)
  CHECK(1_2, 0_2, T)
  CHECK(0_2, 107_2, F)
  CHECK(5_2, huge(0_2) / 2_2, F)
  CHECK(5_2, huge(0_2), F)
  CHECK(-1_2, 0_2, T)
  CHECK(0_2, -19_2, F)
  CHECK(huge(0_2), -19_2, F)

  CHECK(0_4, 0_4, T)
  CHECK(1_4, 0_4, T)
  CHECK(0_4, 107_4, F)
  CHECK(5_4, huge(0_4) / 2_4, F)
  CHECK(5_4, huge(0_4), F)
  CHECK(-1_4, 0_4, T)
  CHECK(0_4, -19_4, F)
  CHECK(huge(0_4), -19_4, F)

  CHECK(0_8, 0_8, T)
  CHECK(1_8, 0_8, T)
  CHECK(0_8, 107_8, F)
  CHECK(5_8, huge(0_8) / 2_8, F)
  CHECK(5_8, huge(0_8), F)
  CHECK(-1_8, 0_8, T)
  CHECK(0_8, -19_8, F)
  CHECK(huge(0_8), -19_8, F)

contains

  pure logical function run_bge1 (i, j) result(res)
    integer(kind=1), intent(in) :: i, j
    res = bge(i,j)
  end function
  pure logical function run_bgt1 (i, j) result(res)
    integer(kind=1), intent(in) :: i, j
    res = bgt(i,j)
  end function
  pure logical function run_ble1 (i, j) result(res)
    integer(kind=1), intent(in) :: i, j
    res = ble(i,j)
  end function
  pure logical function run_blt1 (i, j) result(res)
    integer(kind=1), intent(in) :: i, j
    res = blt(i,j)
  end function

  pure logical function run_bge2 (i, j) result(res)
    integer(kind=2), intent(in) :: i, j
    res = bge(i,j)
  end function
  pure logical function run_bgt2 (i, j) result(res)
    integer(kind=2), intent(in) :: i, j
    res = bgt(i,j)
  end function
  pure logical function run_ble2 (i, j) result(res)
    integer(kind=2), intent(in) :: i, j
    res = ble(i,j)
  end function
  pure logical function run_blt2 (i, j) result(res)
    integer(kind=2), intent(in) :: i, j
    res = blt(i,j)
  end function

  pure logical function run_bge4 (i, j) result(res)
    integer(kind=4), intent(in) :: i, j
    res = bge(i,j)
  end function
  pure logical function run_bgt4 (i, j) result(res)
    integer(kind=4), intent(in) :: i, j
    res = bgt(i,j)
  end function
  pure logical function run_ble4 (i, j) result(res)
    integer(kind=4), intent(in) :: i, j
    res = ble(i,j)
  end function
  pure logical function run_blt4 (i, j) result(res)
    integer(kind=4), intent(in) :: i, j
    res = blt(i,j)
  end function

  pure logical function run_bge8 (i, j) result(res)
    integer(kind=8), intent(in) :: i, j
    res = bge(i,j)
  end function
  pure logical function run_bgt8 (i, j) result(res)
    integer(kind=8), intent(in) :: i, j
    res = bgt(i,j)
  end function
  pure logical function run_ble8 (i, j) result(res)
    integer(kind=8), intent(in) :: i, j
    res = ble(i,j)
  end function
  pure logical function run_blt8 (i, j) result(res)
    integer(kind=8), intent(in) :: i, j
    res = blt(i,j)
  end function

end
