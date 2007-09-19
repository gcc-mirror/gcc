! { dg-do compile }
! { dg-options "-fopenmp -fcray-pointer" }
! { dg-require-effective-target tls_native }

module crayptr2
  integer :: e		! { dg-error "CRAY POINTEE attribute conflicts with THREADPRIVATE" }
  pointer (ip5, e)

! The standard is not very clear about this.
! Certainly, Cray pointees can't be SAVEd, nor they can be
! in COMMON, so the only way to make threadprivate Cray pointees would
! be if they are module variables.  But threadprivate pointees don't
! make any sense anyway.

!$omp threadprivate (e)

end module crayptr2
