! PR fortran/100642
! Contributed by G. Steinmetz
program p
   !use omp_lib, only: omp_depend_kind
   use iso_c_binding, only: c_intptr_t
   integer, parameter :: omp_depend_kind = 2*c_intptr_t
   integer(omp_depend_kind) :: a, b
   !$acc data
   !$omp depobj(b) depend(out:a)  ! { dg-error "The !\\\$OMP DEPOBJ directive cannot be specified within a !\\\$ACC DATA region" }
   !$acc end data
end
