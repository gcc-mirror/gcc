! In OpenMP 5.2 permits tofrom for enter/exit data
! in the FE, it is already converted to 'to' and 'from', respectively.
module m
  integer :: y, z
contains
subroutine copyin
  !$omp target enter data map(from: y)         ! { dg-error "TARGET ENTER DATA with map-type other than TO, TOFROM or ALLOC on MAP clause" }
  !$omp target enter data map(always, from: z) ! { dg-error "TARGET ENTER DATA with map-type other than TO, TOFROM or ALLOC on MAP clause" }
end
subroutine copyout
  !$omp target exit data map(to: y)         ! { dg-error "TARGET EXIT DATA with map-type other than FROM, TOFROM, RELEASE, or DELETE on MAP clause" }
  !$omp target exit data map(always, to: z) ! { dg-error "TARGET EXIT DATA with map-type other than FROM, TOFROM, RELEASE, or DELETE on MAP clause" }
end
end
