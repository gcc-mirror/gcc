! { dg-do run }
!
! Test move_alloc for polymorphic scalars
!
! The following checks that a move_alloc from
! a TYPE to a CLASS works
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
    type(extended_type), allocatable :: tmp

   allocate (tmp)

   if (tmp%i /= 2 .or. tmp%j /= 77) STOP 1
   tmp%i = 5
   tmp%j = 88

   select type(a)
     type is(base_type)
       if (a%i /= -44) STOP 2
       a%i = -99
     class default
       STOP 3
   end select

   call move_alloc (from=tmp, to=a)

   select type(a)
     type is(extended_type)
       if (a%i /= 5) STOP 4
       if (a%j /= 88) STOP 5
       a%i = 123
       a%j = 9498
     class default
       STOP 6
   end select

   if (allocated (tmp)) STOP 7
  end subroutine myallocate
end module myalloc

program main
  use myalloc
  implicit none
  class(base_type), allocatable :: a

  allocate (a)

  select type(a)
    type is(base_type)
      if (a%i /= 2) STOP 8
      a%i = -44
    class default
      STOP 9
  end select

  call myallocate (a)

  select type(a)
    type is(extended_type)
      if (a%i /= 123) STOP 10
      if (a%j /= 9498) STOP 11
    class default
      STOP 12
  end select
end program main
