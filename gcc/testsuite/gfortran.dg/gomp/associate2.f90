! { dg-do compile }
!
! PR fortran/103039
!

subroutine shared_test(cc, ar)
implicit none
class(*) :: cc
integer :: ar(..)

associate(aa => cc)
  !$omp parallel shared(aa)  ! { dg-error "Associate name 'aa' in SHARED clause" }
  !$omp end parallel
end associate

select type(tt => cc)
  type is (integer)
  !$omp parallel shared(tt) ! { dg-error "Associate name 'tt' in SHARED clause" }
  !$omp end parallel
end select

select type(cc)
  type is (integer)
  !$omp parallel shared(cc) ! { dg-error "Associate name 'cc' in SHARED clause" }
  !$omp end parallel
end select

select rank(rr => ar)
  rank(1)
  !$omp parallel shared(rr) ! { dg-error "Associate name 'rr' in SHARED clause" }
  !$omp end parallel
end select

select rank(ar)
  rank(1)
  !$omp parallel shared(ar) ! { dg-error "Associate name 'ar' in SHARED clause" }
  !$omp end parallel
end select
end



subroutine firstprivate_test(cc, ar)
implicit none
class(*) :: cc
integer :: ar(..)

associate(aa => cc)
  !$omp parallel firstprivate(aa)  ! { dg-error "Associate name 'aa' in FIRSTPRIVATE clause" }
  !$omp end parallel
end associate

select type(tt => cc)
  type is (integer)
  !$omp parallel firstprivate(tt) ! { dg-error "Associate name 'tt' in FIRSTPRIVATE clause" }
  !$omp end parallel
end select

select type(cc)
  type is (integer)
  !$omp parallel firstprivate(cc) ! { dg-error "Associate name 'cc' in FIRSTPRIVATE clause" }
  !$omp end parallel
end select

select rank(rr => ar)
  rank(1)
  !$omp parallel firstprivate(rr) ! { dg-error "Associate name 'rr' in FIRSTPRIVATE clause" }
  !$omp end parallel
end select

select rank(ar)
  rank(1)
  !$omp parallel firstprivate(ar) ! { dg-error "Associate name 'ar' in FIRSTPRIVATE clause" }
  !$omp end parallel
end select
end
