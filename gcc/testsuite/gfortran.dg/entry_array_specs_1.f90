! { dg-do compile }
! Tests the fix for PR25091 and PR25092 in which mismatched array
! specifications between entries of the same procedure were not diagnosed.

! Contributed by Joost VandeVondele  <jv244@cam.ac.uk> 

! This was PR25091 - no diagnostic given on error
 FUNCTION F1() RESULT(RES_F1) ! { dg-error "mismatched array specifications" }
 INTEGER RES_F1(2,2)
 INTEGER RES_E1(4)
 ENTRY E1() RESULT(RES_E1)
 END FUNCTION

! This was PR25092 - no diagnostic given on error
 FUNCTION F2() RESULT(RES_F2) ! { dg-error "mismatched array specifications" }
 INTEGER :: RES_F2(4)
 INTEGER :: RES_E2(3)
 ENTRY E2() RESULT(RES_E2)
 END FUNCTION

! Check that the versions without explicit results give the error
 FUNCTION F3() ! { dg-error "mismatched array specifications" }
 INTEGER :: F3(4)
 INTEGER :: E3(2,2)
 ENTRY E3()
 END FUNCTION

 FUNCTION F4() ! { dg-error "mismatched array specifications" }
 INTEGER :: F4(4)
 INTEGER :: E4(3)
 ENTRY E4()
 END FUNCTION

! Check that conforming entries are OK.
 FUNCTION F5()
 INTEGER :: F5(4,5,6)
 INTEGER :: E5(4,5,6)
 ENTRY E5()
 END FUNCTION
