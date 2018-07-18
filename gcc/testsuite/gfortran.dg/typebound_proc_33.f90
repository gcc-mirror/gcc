! { dg-do compile }
!
! PR 60232: [OOP] The rank of the element in the structure constructor does not match that of the component
!
! Contributed by Antony Lewis <antony@cosmologist.info>

module ObjectLists
  implicit none

  Type TObjectList
  contains
    procedure :: ArrayItem
  end Type

contains

  function ArrayItem(L) result(P)
    Class(TObjectList) :: L
    Class(TObjectList), pointer :: P(:)
  end function

end module


  use ObjectLists
  implicit none

  Type, extends(TObjectList):: TSampleList
  end Type

contains

  subroutine TSampleList_ConfidVal(L)
    Class(TSampleList) :: L
  end subroutine

end
