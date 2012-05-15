! { dg-do run }
!
! PR 42769: [OOP] ICE in resolve_typebound_procedure
! comment #27
!
! Contributed by Janus Weil <janus@gcc.gnu.org>


module mod1
  type :: t1
  contains
    procedure, nopass :: get => my_get
  end type
contains 
  integer function my_get()
    my_get = 1
  end function
end module

module mod2
contains 
  integer function my_get()   ! must have the same name as the function in mod1
    my_get = 2
  end function
end module

  use mod2
  use mod1              ! order of use statements is important
  class(t1),allocatable :: a
  allocate(a)
  if (a%get()/=1) call abort()
end
