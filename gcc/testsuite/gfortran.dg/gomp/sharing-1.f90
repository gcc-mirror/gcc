! { dg-do compile }
! { dg-require-effective-target tls }

  integer :: thrpriv, thr, i, j, s, g1, g2, m
  integer, dimension (6) :: p
  common /thrblk/ thr
  common /gblk/ g1
  save thrpriv, g2
!$omp threadprivate (/thrblk/, thrpriv)
  s = 1
!$omp parallel do default (none) &
!$omp & private (p) shared (s) ! { dg-error "enclosing parallel" }
  do i = 1, 64
    call foo (thrpriv)	! Predetermined - threadprivate
    call foo (thr)	! Predetermined - threadprivate
    call foo (i)	! Predetermined - omp do iteration var
    do j = 1, 64	! Predetermined - sequential loop
      call foo (j)	! iteration variable
    end do
    call bar ((/ (k * 4, k = 1, 8) /)) ! Predetermined - implied do
    forall (l = 1 : i) &! Predetermined - forall indice
      p(l) = 6		! Explicitly determined - private
    call foo (s)	! Explicitly determined - shared
    call foo (g1)	! { dg-error "not specified in" }
    call foo (g2)	! { dg-error "not specified in" }
    call foo (m)	! { dg-error "not specified in" }
  end do
end
