! PR fortran/10843
! Origin: Brad Davis <bdavis9659@comcast.net>
!
! { dg-do compile }
! { dg-options "-ffixed-form" }
      GO TO 3
      GOTO 3
 3    CONTINUE
      GOTO = 55
      GO TO = 55
      END

