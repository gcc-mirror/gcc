! { dg-do compile }
! This checks the fix for PR20889 in wrong pointer types in derived
! type constructors would either give no message or would segfault.
!
! Contributed by Joost VandVondele  <jv244@cam.ac.uk>
!==============
  TYPE TEST
    REAL, POINTER :: A
  END TYPE

  TYPE TEST1
    REAL :: A
  END TYPE

  INTEGER, POINTER :: IP
  real, POINTER :: RP
  TYPE(TEST) :: DD
  TYPE(TEST1) :: EE
! Next line is the original => gave no warning/error.
  DD=TEST(NULL(IP))    ! { dg-error "INTEGER but should be REAL" }
! Would segfault here.
  DD=TEST(IP)          ! { dg-error "INTEGER but should be REAL" }
! Check right target type is OK.
  DD=TEST(NULL(RP))
! Check non-pointer is OK.
  EE= TEST1(1)
! Test attempted conversion from character to real.
  EE= TEST1("e")       ! { dg-error "convert CHARACTER" }
END