! { dg-do compile }
! PR 96386 - this used to cause an ICE.
! Test case by Menno Deij - van Rijswijk.

MODULE assoc

TYPE Level3
  INTEGER                      :: someNumber
END TYPE Level3

TYPE Level2
  INTEGER                      :: nLevel3
  TYPE (Level3), ALLOCATABLE   :: levels3(:)
  
END TYPE Level2

TYPE Level1
  INTEGER                      :: nLevel2
  TYPE (Level2), ALLOCATABLE   :: levels2(:)
END TYPE Level1

TYPE outer_type
  INTEGER                      :: nLevel1
  TYPE (Level1), ALLOCATABLE   :: levels1(:)
END TYPE outer_type

TYPE(outer_type), TARGET       :: outer

CONTAINS

SUBROUTINE internal_compiler_error_repro()

INTEGER F,B,Z

ASSOCIATE(l1 => outer%levels1 ) ! <-- this gives an ICE
!ASSOCIATE(l1 => outer%levels1(:) )  ! <-- No ICE if array spec is added
  DO F=1,outer%nLevel1
     ASSOCIATE(l2 => l1(F)%levels2 )
       DO B=1,l2(F)%nLevel3 ! <-- condition for ICE to be triggered

      END DO
     END ASSOCIATE
  END DO
END ASSOCIATE 

END SUBROUTINE internal_compiler_error_repro
end module
