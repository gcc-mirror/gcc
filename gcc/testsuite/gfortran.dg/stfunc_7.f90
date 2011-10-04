! { dg-do compile }
! { dg-options "-std=legacy" }
!
! PR 50553: statement function cannot be target (r178939)
!
! Contributed by Vittorio Zecca <zeccav@gmail.com>

f(x)=x
target f  ! { dg-error "attribute conflicts with" }
end
