! { dg-do run }
!
! PR 48699: [4.6/4.7 Regression] [OOP] MOVE_ALLOC inside SELECT TYPE
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>

program testmv1

  type bar
  end type

  type, extends(bar) ::  bar2
  end type

  class(bar), allocatable :: sm
  type(bar2), allocatable :: sm2

  allocate (sm2)
  call move_alloc (sm2,sm)

  if (allocated(sm2)) STOP 1
  if (.not. allocated(sm)) STOP 2

end program 
