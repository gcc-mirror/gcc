! { dg-do compile }
! { dg-options "-fopenmp" }
!
! PR fortran/60127
!
! OpenMP 4.0 doesn't permit DO CONCURRENT (yet)
!

!$omp do
do concurrent(i=1:5) ! { dg-error "OMP DO cannot be a DO CONCURRENT loop" }
print *, 'Hello'
end do
end
