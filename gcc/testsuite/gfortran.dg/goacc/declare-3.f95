! Test valid usage of the OpenACC 'declare' directive.

module mod_a
  implicit none
  integer :: a
  !$acc declare create (a)
end module

module mod_b
  implicit none
  integer :: b
  !$acc declare copyin (b)
end module

module mod_c
  implicit none
  integer :: c
  !$acc declare deviceptr (c)
end module

module mod_d
  implicit none
  integer :: d
  !$acc declare device_resident (d)
end module

module mod_e
  implicit none
  integer :: e
  !$acc declare link (e)
end module

subroutine sub1
  use mod_a
  use mod_b
  use mod_c
  use mod_d
  use mod_e
end subroutine sub1

program test
  use mod_a
  use mod_b
  use mod_c
  use mod_d
  use mod_e
end program test
