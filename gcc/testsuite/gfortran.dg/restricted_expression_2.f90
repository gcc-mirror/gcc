! { dg-do compile }
! { dg-options "-pedantic -ffixed-form" }

! PR fortran/35723
! Check that a program using a local variable subscript is still rejected.

! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

      call       vf0016(  1,  2,  3)

      end
      SUBROUTINE VF0016(nf1,nf2,nf3)
      CHARACTER(LEN=9,KIND=1),DIMENSION(3), PARAMETER
     $     ::  TEST_STRINGS =
     $  (/'       HI','ABC      ','  CDEFG  '/)
      INTEGER :: i = 2
      CHARACTER :: TEST_ARRAY
     $(LEN_TRIM(ADJUSTL(TEST_STRINGS(i))), ! { dg-error "'i' cannot appear" }
     $ SUM(LEN_TRIM(ADJUSTL(TEST_STRINGS))),
     $ LEN_TRIM(ADJUSTL(ADJUSTR(TEST_STRINGS(3)))),
     $ SUM(LEN_TRIM(ADJUSTL(ADJUSTR(TEST_STRINGS(NF1:NF3:NF2)))))   )

       print *, 2, 10, 5, 7
       print *, shape (test_array)
         end
