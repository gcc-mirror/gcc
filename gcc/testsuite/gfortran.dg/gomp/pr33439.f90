! PR fortran/33439
! { dg-do compile }
! { dg-options "-fopenmp" }

subroutine pr33439_1
  integer :: s, i
  s = 4
!$omp parallel default(none)	! { dg-message "note: enclosing 'parallel'" }
  call somethingelse
!$omp do schedule(static, s)	! { dg-error "not specified in enclosing 'parallel'" }
  do i = 1, 8
    call something
  end do
!$omp end do
!$omp end parallel
end subroutine pr33439_1

subroutine pr33439_2
  integer :: s, i
  s = 4
!$omp parallel default(none)	! { dg-message "note: enclosing 'parallel'" }
!$omp do schedule(static, s)	! { dg-error "not specified in enclosing 'parallel'" }
  do i = 1, 8
    call something
  end do
!$omp end do
!$omp end parallel
end subroutine pr33439_2

subroutine pr33439_3
  integer :: s, i
  s = 4
!$omp parallel do default(none) schedule(static, s)	! { dg-message "note: enclosing 'parallel'" }
!							! { dg-error "'s' not specified in enclosing 'parallel'" "" { target *-*-* } .-1 }
  do i = 1, 8
    call something
  end do
!$omp end parallel do
end subroutine pr33439_3
