! Test valid usage of the OpenACC 'declare' directive.

! { dg-additional-options "-fdump-tree-original" }

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
  use mod_d
  use mod_e
end subroutine sub1

program test
  use mod_a
  use mod_b
  use mod_d
  use mod_e

  ! { dg-final { scan-tree-dump {(?n)#pragma acc data map\(force_alloc:d\) map\(force_to:b\) map\(force_alloc:a\)$} original } }
end program test

! { dg-final { scan-tree-dump-times {#pragma acc data} 1 original } }
