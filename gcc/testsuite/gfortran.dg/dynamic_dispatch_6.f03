! { dg-do run }
!
! PR 42144: [OOP] deferred TBPs do not work
!
! Contributed by Damian Rouson <damian@rouson.net>

module field_module
  implicit none
  private
  public :: field
  type ,abstract :: field 
  end type
end module

module periodic_5th_order_module
  use field_module ,only : field
  implicit none
  type ,extends(field) :: periodic_5th_order
  end type
end module

module field_factory_module
  implicit none
  private
  public :: field_factory
  type, abstract :: field_factory 
  contains 
    procedure(create_interface), deferred :: create 
  end type 
  abstract interface 
    function create_interface(this) 
      use field_module ,only : field
      import :: field_factory
      class(field_factory), intent(in) :: this 
      class(field) ,pointer :: create_interface
    end function
  end interface 
end module

module periodic_5th_factory_module
  use field_factory_module , only : field_factory
  implicit none
  private
  public :: periodic_5th_factory
  type, extends(field_factory) :: periodic_5th_factory 
  contains 
    procedure :: create=>new_periodic_5th_order
  end type 
contains
  function new_periodic_5th_order(this) 
    use field_module ,only : field
    use periodic_5th_order_module ,only : periodic_5th_order
    class(periodic_5th_factory), intent(in) :: this
    class(field) ,pointer :: new_periodic_5th_order
  end function
end module

program main 
  use field_module ,only : field 
  use field_factory_module ,only : field_factory
  use periodic_5th_factory_module ,only : periodic_5th_factory
  implicit none 
  class(field) ,pointer :: u
  class(field_factory), allocatable :: field_creator 
  allocate (periodic_5th_factory ::  field_creator) 
  u => field_creator%create() 
end program

! { dg-final { cleanup-modules "field_module periodic_5th_order_module field_factory_module periodic_5th_factory_module" } }
