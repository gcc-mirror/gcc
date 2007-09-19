! { dg-do compile }
! { dg-require-effective-target tls_native }
      subroutine bad1
	double precision :: d	! { dg-error "isn't SAVEd" }
!$omp threadprivate (d)
      end subroutine bad1
