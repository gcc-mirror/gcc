! { dg-do compile }
! PR 52668 - there used to be a bogus warning about not using b.
! Original test case by Arnaud Desitter.
module mm
  integer :: a, b
  common /mm1/ a, b
end module mm

subroutine aa()
  use mm, only: a
  implicit none
  a = 1
end subroutine aa
! { dg-final { cleanup-modules "mm" } }
