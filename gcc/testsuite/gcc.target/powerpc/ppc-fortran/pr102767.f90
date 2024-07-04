! { dg-options "-mvsx -O2 -ftree-vectorize -mno-efficient-unaligned-vsx" }
! { dg-require-effective-target powerpc_vsx }

INTERFACE
  FUNCTION elemental_mult (a, b, c)
    type(*), DIMENSION(..) :: a, b, c
  END
END INTERFACE

allocatable  z
integer, dimension(2,2) :: a, b
call test_CFI_address
contains
  subroutine test_CFI_address
    if (elemental_mult (z, x, y) .ne. 0) stop
    a = reshape ([4,3,2,1], [2,2])
    b = reshape ([2,3,4,5], [2,2])
    if (elemental_mult (i, a, b) .ne. 0) stop
  end
end

