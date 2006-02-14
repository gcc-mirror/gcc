! { dg-do compile }
! { dg-options "-fopenmp -fcray-pointer" }

  integer :: a, b, c, d, i
  pointer (ip1, a)
  pointer (ip2, b)
  pointer (ip3, c)
  pointer (ip4, d)

!$omp parallel shared (a)	! { dg-error "Cray pointee 'a' in SHARED clause" }
!$omp end parallel

!$omp parallel private (b)	! { dg-error "Cray pointee 'b' in PRIVATE clause" }
!$omp end parallel

!$omp parallel firstprivate (c)	! { dg-error "Cray pointee 'c' in FIRSTPRIVATE clause" }
!$omp end parallel

!$omp parallel do lastprivate (d) ! { dg-error "Cray pointee 'd' in LASTPRIVATE clause" }
  do i = 1, 10
    if (i .eq. 10) d = 1
  end do
!$omp end parallel do

!$omp parallel reduction (+: a)	! { dg-error "Cray pointee 'a' in REDUCTION clause" }
!$omp end parallel

  ip1 = loc (i)
!$omp parallel shared (ip1)
  a = 2
!$omp end parallel

!$omp parallel private (ip2, i)
  ip2 = loc (i)
  b = 1
!$omp end parallel

  ip3 = loc (i)
!$omp parallel firstprivate (ip3) ! { dg-error "Cray pointer 'ip3' in FIRSTPRIVATE clause" }
!$omp end parallel

!$omp parallel do lastprivate (ip4) ! { dg-error "Cray pointer 'ip4' in LASTPRIVATE clause" }
  do i = 1, 10
    if (i .eq. 10) ip4 = loc (i)
  end do
!$omp end parallel do

!$omp parallel reduction (+: ip1) ! { dg-error "Cray pointer 'ip1' in REDUCTION clause" }
!$omp end parallel

end
