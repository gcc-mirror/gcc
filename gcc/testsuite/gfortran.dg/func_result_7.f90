! { dg-do compile }
!
! PR 50073: gfortran must not accept function name when result name is present
!
! Contributed by Vittorio Zecca <zeccav@gmail.com>

function fun() result(f)  ! { dg-error "RESULT variable" } 
  pointer fun             ! { dg-error "RESULT variable" }
  dimension fun(1)        ! { dg-error "RESULT variable" }
  f=0
end
