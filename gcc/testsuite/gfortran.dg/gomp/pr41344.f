      subroutine xrotate(nerr)

      common /dfm/ndfl

*$omp parallel private(ix)
      ix = 0
*$omp do
      do i=1,ndfl
         ix = ix + 1
	 if (ix.gt.5) go to 9000 ! { dg-error "invalid (exit|branch)" }
      enddo
*$omp end do
*$omp end parallel

9000  continue
      end
