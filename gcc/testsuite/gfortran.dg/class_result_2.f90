! { dg-do compile }
!
! PR 59414: [OOP] Class array pointers: compile error on valid code (Different ranks in pointer assignment)
!
! Contributed by Antony Lewis <antony@cosmologist.info>

    implicit none

    Type TObjectList
    end Type

    Class(TObjectList), pointer :: Arr(:)
    Arr => ArrayItem()
      
  contains

    function ArrayItem() result(P)
      Class(TObjectList), pointer :: P(:)
    end function

end
