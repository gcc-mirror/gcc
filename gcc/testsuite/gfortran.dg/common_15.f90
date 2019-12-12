! { dg-do compile }
!
! PR 50515: gfortran should not accept an external that is a common (r178939)
!
! Contributed by Vittorio Zecca <zeccav@gmail.com>

common/sub/ a  ! { dg-error "cannot have the EXTERNAL attribute" }
external sub
end
