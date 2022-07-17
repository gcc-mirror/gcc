! { dg-do compile }
! { dg-options "-fdump-tree-original" }
! PR fortran/105954 - ICE in gfc_element_size, at fortran/target-memory.cc:132
! Contributed by G.Steinmetz

program p
  use iso_c_binding, only: c_float, c_sizeof
  implicit none
  integer, parameter :: n = -99
  type t
     real :: b(3,7:n)
  end type
  type, bind(c) :: u
     real(c_float) :: b(3,7:n)
  end type
  type(t) :: d
  type(u) :: e
  integer, parameter :: k = storage_size(d)
  integer, parameter :: m = sizeof(d)
  integer, parameter :: l = c_sizeof(e)
  if (k /= 0) stop 1
  if (m /= 0) stop 2
  if (l /= 0) stop 3
end

! { dg-final { scan-tree-dump-not "_gfortran_stop_numeric" "original" } }
