! PR fortran/77500
! { dg-do compile }

program pr77500
   real :: x
!$omp atomic write
   x = f()
!$omp end atomic
end
