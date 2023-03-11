! { dg-do compile }
! { dg-options "-fcoarray=single -fcheck=bounds -ftrapv" }
! PR fortran/106945
! Contributed by G. Steinmetz

module m
  implicit none
  type t
     class(*), allocatable :: a[:]
  end type
end
