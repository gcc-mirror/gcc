! { dg-do compile }
! { dg-options "-pedantic -ffixed-form" }

! PR fortran/35723
! An argument subscript into a parameter array was not allowed as
! dimension.  Check this is fixed.

! Contributed by Dick Hendrickson <dick.hendrickson@gmail.com>

      call       vf0016(  1,  2,  3)

      end
      SUBROUTINE VF0016(nf1,nf2,nf3)
      CHARACTER(LEN=9,KIND=1),DIMENSION(3), PARAMETER
     $     ::  TEST_STRINGS =
     $  (/'       HI','ABC      ','  CDEFG  '/)
      CHARACTER :: TEST_ARRAY
     $(LEN_TRIM(ADJUSTL(TEST_STRINGS(nf1))),
     $ SUM(LEN_TRIM(ADJUSTL(TEST_STRINGS))),
     $ LEN_TRIM(ADJUSTL(ADJUSTR(TEST_STRINGS(3)))),
     $ SUM(LEN_TRIM(ADJUSTL(ADJUSTR(TEST_STRINGS(NF1:NF3:NF2)))))   )

       print *, 2, 10, 5, 7
       print *, shape (test_array)
         end
