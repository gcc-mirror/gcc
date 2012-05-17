! { dg-do compile }
!
! PR 47768: printing a derived-type variable with proc-pointer components
!
! Contributed by Arjen Markus <arjen.markus895@gmail.com>

module proc_pointers
  implicit none
  type :: rectangle
    real :: width, height
    procedure(real), pointer, nopass :: get_special_area
  end type
end module

program test_objects
  use proc_pointers
  implicit none
  type(rectangle) :: rect
  write(*,*) rect          ! { dg-error "cannot have procedure pointer components" }
end program
