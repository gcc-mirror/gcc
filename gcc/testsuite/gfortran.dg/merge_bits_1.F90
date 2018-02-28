! Test the MERGE_BITS intrinsic
!
! { dg-do run }
! { dg-options "-ffree-line-length-none" }

  interface run_merge
    procedure run_merge_1
    procedure run_merge_2
    procedure run_merge_4
    procedure run_merge_8
  end interface

#define CHECK(I,J,K) \
  if (merge_bits(I,J,K) /= ior(iand(I,K),iand(J,not(K)))) STOP 1; \
  if (run_merge(I,J,K) /= merge_bits(I,J,K)) STOP 2

  CHECK(13_1,18_1,22_1)
  CHECK(-13_1,18_1,22_1)
  CHECK(13_1,-18_1,22_1)
  CHECK(13_1,18_1,-22_1)

  CHECK(13_2,18_2,22_2)
  CHECK(-13_2,18_2,22_2)
  CHECK(13_2,-18_2,22_2)
  CHECK(13_2,18_2,-22_2)

  CHECK(13_4,18_4,22_4)
  CHECK(-13_4,18_4,22_4)
  CHECK(13_4,-18_4,22_4)
  CHECK(13_4,18_4,-22_4)

  CHECK(13_8,18_8,22_8)
  CHECK(-13_8,18_8,22_8)
  CHECK(13_8,-18_8,22_8)
  CHECK(13_8,18_8,-22_8)

contains

  function run_merge_1 (i, j, k) result(res)
    integer(kind=1) :: i, j, k, res
    res = merge_bits(i,j,k)
  end function 
  function run_merge_2 (i, j, k) result(res)
    integer(kind=2) :: i, j, k, res
    res = merge_bits(i,j,k)
  end function 
  function run_merge_4 (i, j, k) result(res)
    integer(kind=4) :: i, j, k, res
    res = merge_bits(i,j,k)
  end function 
  function run_merge_8 (i, j, k) result(res)
    integer(kind=8) :: i, j, k, res
    res = merge_bits(i,j,k)
  end function 
end
