! { dg-do compile }
!
! PR 56081: [4.7/4.8 Regression] Segfault ICE on select with bad case
!
! Contributed by Richard L Lozes <richard@lozestech.com>

  implicit none
  integer :: a(4)
  select case(a)   ! { dg-error "must be a scalar expression" }
  case (0)
  end select
end
