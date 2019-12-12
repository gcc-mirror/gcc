! { dg-do compile }
! { dg-options "-Og -ffrontend-optimize -fcheck=bounds -fdump-tree-optimized" }
! Check that bounds checking is done only before the matrix
! multiplication.

module y
contains
  subroutine x(a,b,c)
    real, dimension(:,:) :: a, b, c
    c = matmul(a,b)
  end subroutine x
end module y
! { dg-final { scan-tree-dump-times "_runtime_error" 3 "optimized" } }
