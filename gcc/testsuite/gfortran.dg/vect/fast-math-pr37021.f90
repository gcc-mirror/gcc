! { dg-do compile }
! { dg-require-effective-target vect_double }

subroutine to_product_of(self,a,b,a1,a2)
  complex(kind=8) :: self (:)
  complex(kind=8), intent(in) :: a(:,:)
  complex(kind=8), intent(in) :: b(:)
  integer a1,a2
  self = ZERO
  do i = 1,a1
    do j = 1,a2
      self(i) = self(i) + a(i,j)*b(j)
    end do
  end do
end subroutine

! { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } }
! { dg-final { cleanup-tree-dump "vect" } }
