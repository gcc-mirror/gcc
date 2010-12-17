! { dg-require-effective-target tls }
      module omp_threadprivate1
	common /T/ a
      end module omp_threadprivate1
      subroutine bad1
	use omp_threadprivate1
!$omp threadprivate (/T/)	! { dg-error "not found" }
      end subroutine bad1
      subroutine bad2
	common /S/ b
!$omp threadprivate (/S/)
      contains
	subroutine bad3
!$omp parallel copyin (/T/)	! { dg-error "not found" }
!$omp end parallel		! { dg-error "" }
	end subroutine bad3
      end subroutine bad2
! { dg-final { cleanup-modules "omp_threadprivate1" } }
