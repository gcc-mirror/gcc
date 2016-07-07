! PR fortran/71704
! { dg-do compile }

real function f0 ()
!$omp declare simd (f0)
  f0 = 1
end

real function f1 ()
!$omp declare target (f1)
  f1 = 1
end

real function f2 ()
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) &
!$omp & initializer (omp_priv = 0)
  f2 = 1
end

real function f3 ()
  real, save :: t
!$omp threadprivate (t)
  f3 = 1
end

real function f4 ()
!$omp taskwait
  f4 = 1
end

real function f5 ()
!$omp barrier
  f5 = 1
end

real function f6 ()
!$omp parallel
!$omp end parallel
  f6 = 1
end

real function f7 ()
!$omp single
!$omp end single
  f7 = 1
end

real function f8 ()
!$omp critical
!$omp end critical
  f8 = 1
end

real function f9 ()
!$omp critical
!$omp end critical
  f9 = 1
end
