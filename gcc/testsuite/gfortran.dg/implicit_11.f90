! { dg-do compile }
! 
! PR fortran/34760
! The problem with implict typing is that it is unclear
! whether an existing symbol is a variable or a function.
! Thus it remains long FL_UNKNOWN, which causes extra 
! problems; it was failing here since ISTAT was not
! FL_VARIABLE but still FL_UNKNOWN.
!
! Test case contributed by Dick Hendrickson.
!
     MODULE TESTS
       PRIVATE :: ISTAT
       PUBLIC :: ISTAT2
     CONTAINS
     SUBROUTINE AD0001
     REAL RLA1(:)
     ALLOCATABLE RLA1
     ISTAT = -314
     ALLOCATE (RLA1(NF10), STAT = ISTAT)
     ALLOCATE (RLA1(NF10), STAT = ISTAT2)
     END SUBROUTINE
     END MODULE

     MODULE TESTS2
       PRIVATE :: ISTAT2
     CONTAINS
     function istat2()
       istat2 = 0
     end function istat2
     SUBROUTINE AD0001
       REAL RLA1(:)
       ALLOCATABLE RLA1
       ALLOCATE (RLA1(NF10), STAT = ISTAT2) ! { dg-error "must be a variable" }
     END SUBROUTINE
     END MODULE tests2

! { dg-final { cleanup-modules "TESTS" } }
