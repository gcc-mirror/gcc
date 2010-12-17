! { dg-do run }
!
! PR 46581: [4.6 Regression] [OOP] segfault in SELECT TYPE with associate-name
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>


  implicit none

  type :: t1
    integer, allocatable :: ja(:)
  end type

  class(t1), allocatable  :: a 

  allocate(a)

  select type (aa=>a)
  type is (t1)
    if (allocated(aa%ja)) call abort()
  end select

end
