! { dg-do compile }
!
! PR 47728: [OOP] ICE on invalid CLASS declaration
!
! Contributed by Arjen Markus <arjen.markus@deltares.nl>

program test_objects

   implicit none

   type, abstract :: shape
   end type

   type, extends(shape) :: rectangle
       real :: width, height
   end type

   class(shape), dimension(2) :: object  ! { dg-error "must be dummy, allocatable or pointer" }

   object(1) = rectangle( 1.0, 2.0 )  ! { dg-error "Unclassifiable statement" }

end program test_objects
