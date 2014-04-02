! { dg-do compile }
!
! PR 60234: [4.9 Regression] [OOP] ICE in generate_finalization_wrapper at fortran/class.c:1883
!
! Contribued by Antony Lewis <antony@cosmologist.info>

module ObjectLists
    implicit none

    Type TObjectList
    contains
      FINAL :: finalize
    end Type

    Type, extends(TObjectList):: TRealCompareList
    end Type

contains

  subroutine finalize(L)
    Type(TObjectList) :: L
  end subroutine


  integer function CompareReal(this)
    Class(TRealCompareList) :: this
  end function

end module

! { dg-final { cleanup-modules "ObjectLists" } }
