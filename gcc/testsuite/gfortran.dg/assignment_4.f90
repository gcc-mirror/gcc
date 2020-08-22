! { dg-do compile }
! { dg-options "-Wall" }
!
! PR 55855: [OOP] incorrect warning with procedure pointer component on pointer-valued base object
!
! Contributed by Andrew Benson <abensonca@gmail.com>

  implicit none
  type :: event
    procedure(logical), pointer, nopass :: task
  end type event
  logical :: r
  type(event), pointer :: myEvent
  allocate(myEvent)
  r=myEvent%task()  ! { dg-warning "uninitialized" }
end 
