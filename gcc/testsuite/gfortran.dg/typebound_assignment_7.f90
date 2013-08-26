! { dg-do run }
!
! PR 57843: [OOP] Type-bound assignment is resolved to non-polymorphic procedure call
!
! Contributed by John <jwmwalrus@gmail.com>

module mod1
  implicit none
  type :: itemType
  contains
    procedure :: the_assignment => assign_itemType
    generic :: assignment(=) => the_assignment
  end type
contains
  subroutine assign_itemType(left, right)
    class(itemType), intent(OUT) :: left
    class(itemType), intent(IN) :: right
  end subroutine
end module

module mod2
  use mod1
  implicit none
  type, extends(itemType) :: myItem
    character(3) :: name = ''
  contains
    procedure :: the_assignment => assign_myItem
  end type
contains
  subroutine assign_myItem(left, right)
    class(myItem), intent(OUT) :: left
    class(itemType), intent(IN) :: right
    select type (right)
    type is (myItem)
      left%name = right%name
    end select
  end subroutine
end module


program test_assign

  use mod2
  implicit none

  class(itemType), allocatable :: item1, item2

  allocate (myItem :: item1)
  select type (item1)
    type is (myItem)
      item1%name = 'abc'
  end select

  allocate (myItem :: item2)
  item2 = item1

  select type (item2)
    type is (myItem)
      if (item2%name /= 'abc') call abort()
    class default
      call abort()
  end select

end

! { dg-final { cleanup-modules "mod1 mod2" } }
