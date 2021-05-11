! { dg-do run }
!
! Test the fix for PR46991 - enable class assumed size arrays
!
! Reported by Tobias Burnus  <burnus@gcc.gnu.org>
! from http://j3-fortran.org/pipermail/j3/2010-December/004084.html
! submitted by Robert Corbett.
!
       MODULE TYPES
         PRIVATE
         PUBLIC REC, REC2

         TYPE REC
           INTEGER A
         END TYPE

         TYPE, EXTENDS(REC) :: REC2
           INTEGER B
         END TYPE
       END

       SUBROUTINE SUB1(A, N)
         USE TYPES
         CLASS(REC), INTENT(IN) :: A(*)
         INTERFACE
           SUBROUTINE SUB2(A, N, IARRAY)
             USE TYPES
             TYPE(REC) A(*)
             INTEGER :: N, IARRAY(N)
           END
         END INTERFACE

         CALL SUB2(A, N,[1,2,2,3,3,4,4,5,5,6])
         select type (B => A(1:N))
             type is (REC2)
                 call SUB2(B%REC,N,[1,2,3,4,5,6,7,8,9,10])
         end select

       END

       SUBROUTINE SUB2(A, N, IARRAY)
         USE TYPES
         TYPE(REC) A(*)
         INTEGER :: N, IARRAY(N)
         if (any (A(:N)%A .ne. IARRAY(:N))) stop 1
       END

       PROGRAM MAIN
         USE TYPES
         CLASS(REC), ALLOCATABLE :: A(:)
         INTERFACE
           SUBROUTINE SUB1(A, N)
             USE TYPES
             CLASS(REC), INTENT(IN) :: A(*)
           END SUBROUTINE
         END INTERFACE

         A = [ (REC2(I, I+1), I = 1, 10) ]
         CALL SUB1(A, 10)
       END
