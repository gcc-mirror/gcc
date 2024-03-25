! { dg-do compile }
! { dg-options "-std=f2023" }
! PR fortran/112609 - F2023 restrictions on integer arguments to SYSTEM_CLOCK

program p
  implicit none
  integer    :: i,  j,  k
  integer(2) :: i2, j2, k2
  integer(8) :: i8, j8, k8
  real       :: x

  call system_clock(count=i2)      ! { dg-error "kind smaller than default integer" }
  call system_clock(count_rate=j2) ! { dg-error "kind smaller than default integer" }
  call system_clock(count_max=k2)  ! { dg-error "kind smaller than default integer" }

  call system_clock(count=i8,count_rate=x,count_max=k8)
  call system_clock(count=i, count_rate=j8)     ! { dg-error "different kind" }
  call system_clock(count=i8,count_rate=j)      ! { dg-error "different kind" }
  call system_clock(count=i, count_max=k8)      ! { dg-error "different kind" }
  call system_clock(count=i8,count_max=k)       ! { dg-error "different kind" }
  call system_clock(count_rate=j, count_max=k8) ! { dg-error "different kind" }
  call system_clock(count_rate=j8,count_max=k)  ! { dg-error "different kind" }
  call system_clock(i,x,k8)                     ! { dg-error "different kind" }
end
