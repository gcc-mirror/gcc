! { dg-do run }
! PR 97799 - this used to segfault intermittently.
! Test case by George Hockney.
      PROGRAM MAIN
      IMPLICIT NONE

      character *(20) CA(4)  ! four cells of length 20

      call CHAR_ENTRY(CA)  ! call char_sub through entry

      write (*,*) CA       ! write result -- not needed for bug
      call CHAR_SUB(CA)    ! call char_sb directly -- not needed
      write (*,*) CA       ! write result -- not needed for bug
      STOP
      END



      SUBROUTINE CHAR_SUB(CARRAY)  ! sets carray cells to 'Something'
      IMPLICIT NONE

      CHARACTER*(*)       CARRAY(*)

      integer i
      integer nelts

      nelts = 4    ! same as size of array in main program
      write (*,*) 'CHAR_SUB'
      write (*,*) 'len(carray(1))', len(carray(1)) ! len is OK at 20
      call flush() ! since the next loop segfaults
      do 1 i=1, nelts  
         CARRAY(i) = 'Something'
  1   continue
      RETURN
      END


      SUBROUTINE             TOP_ENTRY
!
! TOP_ENTRY is never called directly.  It organizes entry points
! and sometimes saves variables for other entry points.  Its
! signature does not matter for the failure
!
      IMPLICIT NONE
!
! Declare input variables for all entry points.  Just one here
!
      CHARACTER*(*)       CARRAY(*)
!
!  Entry point CHAR_ENTRY
!
      ENTRY  CHAR_ENTRY( CARRAY)
      CALL   CHAR_SUB(CARRAY)
      RETURN

      END SUBROUTINE TOP_ENTRY

