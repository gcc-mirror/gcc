! { dg-do compile }
!  Test case from PR61933.
   LOGICAL :: file_exists
   INQUIRE(UNIT=-1,EXIST=file_exists)! { dg-error "cannot be -1" }
   INQUIRE(UNIT=-2,EXIST=file_exists)! { dg-error "cannot be -2" }
END
