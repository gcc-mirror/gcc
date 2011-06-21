! { dg-do compile }
!
! PR 49112: [4.6/4.7 Regression] [OOP] Missing type-bound procedure, "duplicate save" warnings and internal compiler error
!
! Contributed by John <jwmwalrus@gmail.com>

    implicit none
    save

    type :: DateTime
    end type

    class(DateTime), allocatable :: dt

end
