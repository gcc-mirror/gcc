! { dg-do compile }
!
! PR 58185: [4.8/4.9 Regression] [OOP] ICE when selector in SELECT TYPE is non-polymorphic
!
! Contributed by John <jwmwalrus@gmail.com>

  integer :: array
  select type (a => array)   ! { dg-error "Selector shall be polymorphic" }
  end select
end
