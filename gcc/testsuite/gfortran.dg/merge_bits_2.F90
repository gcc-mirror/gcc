! Test the MERGE_BITS intrinsic
!
! { dg-do run }
! { dg-options "-ffree-line-length-none" }
! { dg-require-effective-target fortran_integer_16 }

#define CHECK(I,J,K) \
  if (merge_bits(I,J,K) /= ior(iand(I,K),iand(J,not(K)))) call abort ; \
  if (run_merge(I,J,K) /= merge_bits(I,J,K)) call abort

  CHECK(13_16,18_16,22_16)
  CHECK(-13_16,18_16,22_16)
  CHECK(13_16,-18_16,22_16)
  CHECK(13_16,18_16,-22_16)

contains

  function run_merge (i, j, k) result(res)
    integer(kind=16) :: i, j, k, res
    res = merge_bits(i,j,k)
  end function 
end
