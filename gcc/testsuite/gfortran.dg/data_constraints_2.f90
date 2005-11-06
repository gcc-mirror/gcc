! { dg-do compile }
! { dg-options "-std=f95" }
! Tests constraints for variables in a data statement that are commonly
! relaxed.
!
! Contributed by Paul Thomas <pault@gcc.gnu.org>
!
  common // a
  common /b/ c
  integer d
  data a /1/            ! { dg-error "common block variable" }
  data c /2/            ! { dg-error "common block variable" }
  data d /3/
  data d /4/            ! { dg-error " re-initialization" }
end
