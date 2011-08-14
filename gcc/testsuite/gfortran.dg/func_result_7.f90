! { dg-do compile }
!
! PR 50073: gfortran must not accept function name when result name is present
!
! Contributed by Vittorio Zecca <zeccav@gmail.com>

function fun() result(f)
  pointer fun       ! { dg-error "not allowed" }
  dimension fun(1)  ! { dg-error "not allowed" }
  f=0
end
