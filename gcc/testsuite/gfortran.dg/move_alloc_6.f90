! { dg-do run }
!
! Test move_alloc for polymorphic scalars
!
!
module myalloc
  implicit none

  type :: base_type
     integer :: i  =2
  end type base_type

  type, extends(base_type) :: extended_type
     integer :: j = 77
  end type extended_type
contains
  subroutine myallocate (a)
    class(base_type), allocatable, intent(inout) :: a
    class(base_type), allocatable :: tmp

    allocate (extended_type :: tmp)

    select type(tmp)
      type is(base_type)
        STOP 1
      type is(extended_type)
        if (tmp%i /= 2 .or. tmp%j /= 77) STOP 2
        tmp%i = 5
        tmp%j = 88
    end select

    select type(a)
      type is(base_type)
        if (a%i /= -44) STOP 3
        a%i = -99
      class default
        STOP 4
    end select

    call move_alloc (from=tmp, to=a)

    select type(a)
      type is(extended_type)
        if (a%i /= 5) STOP 5
        if (a%j /= 88) STOP 6
        a%i = 123
        a%j = 9498
      class default
        STOP 7
    end select

    if (allocated (tmp)) STOP 8
  end subroutine myallocate
end module myalloc

program main
  use myalloc
  implicit none
  class(base_type), allocatable :: a

  allocate (a)

  select type(a)
    type is(base_type)
      if (a%i /= 2) STOP 9
      a%i = -44
    class default
      STOP 10
  end select

  call myallocate (a)

  select type(a)
    type is(extended_type)
      if (a%i /= 123) STOP 11
      if (a%j /= 9498) STOP 12
    class default
      STOP 13
  end select
end program main
