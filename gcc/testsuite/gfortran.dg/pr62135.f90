! { dg-do compile }
! { dg-options -Wsurprising }

   PROGRAM PR62135
      IMPLICIT NONE
      CHARACTER*1 :: choice
      choice = 'x'
      SELECT CASE (choice)
         ! This triggered an ICE: an unreachable case clause
         ! as the last of a list.
         CASE ('2':'7','9':'0') ! { dg-warning "can never be matched" }
            WRITE(*,*) "barf"
         CASE DEFAULT
            CONTINUE
      END SELECT
   END PROGRAM PR62135

