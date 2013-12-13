! { dg-do compile }
!
! PR 58916: [F03] Allocation of scalar with array source not rejected
!
! Contributed by Vladimir Fuka <vladimir.fuka@gmail.com>

  class(*), allocatable :: a1
  real, allocatable :: a2  
  real b(1)
  allocate(a1, source=b)  ! { dg-error "must be scalar or have the same rank" }
  allocate(a2, source=b)  ! { dg-error "must be scalar or have the same rank" }
end
