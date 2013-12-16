! { dg-do compile }
!
! PR 59493: [OOP] ICE: Segfault on Class(*) pointer association
!
! Contributed by Hossein Talebi <talebi.hossein@gmail.com>

  implicit none

  type ty_mytype1
  end type

  class(ty_mytype1), allocatable, target:: cla1
  class(*), pointer :: ptr

  ptr => cla1

end
