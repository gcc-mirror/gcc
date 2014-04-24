! { dg-do compile }
!
! PR fortran/60881
!
! Contributed by Damian Rouson
!
! Was ICEing before
!
program main
  implicit none
  type co_object
    logical :: defined=.false.
    real, allocatable :: dummy_to_facilitate_extension[:]
  end type
  type, extends(co_object) :: global_field
  end type
  type(global_field) T
  call assign_local_field(T)
contains
  subroutine assign_local_field(lhs)
    type(global_field) lhs
  end subroutine
end program
