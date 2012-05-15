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

   if (tmp%i /= 2 .or. tmp%j /= 77) call abort()
   tmp%i = 5
   tmp%j = 88

   select type(a)
     type is(base_type)
       if (a%i /= -44) call abort()
       a%i = -99
     class default
       call abort ()
   end select

   call move_alloc (from=tmp, to=a)

   select type(a)
     type is(extended_type)
       if (a%i /= 5) call abort()
       if (a%j /= 88) call abort()
       a%i = 123
       a%j = 9498
     class default
       call abort ()
   end select

   if (allocated (tmp)) call abort()
  end subroutine myallocate
end module myalloc

program main
  use myalloc
  implicit none
  class(base_type), allocatable :: a

  allocate (a)

  select type(a)
    type is(base_type)
      if (a%i /= 2) call abort()
      a%i = -44
    class default
      call abort ()
  end select

  call myallocate (a)

  select type(a)
    type is(extended_type)
      if (a%i /= 123) call abort()
      if (a%j /= 9498) call abort()
    class default
      call abort ()
  end select
end program main
