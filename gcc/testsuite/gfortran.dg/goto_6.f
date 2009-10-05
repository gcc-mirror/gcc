! { dg-do run }
! { dg-options "-w" }

! PR fortran/41403
! Assigned-goto with label list used to compare label addresses which
! failed with optimization.  Check this works correctly now.
! This is the most reduced Fortran code from the PR.

      IVFAIL=0
      ASSIGN 1263 TO I
      GO TO I, (1262,1263,1264)
 1262 ICON01 = 1262
      GO TO 1265
 1263 ICON01 = 1263
      GO TO 1265
 1264 ICON01 = 1264
 1265 CONTINUE
41260 IF ( ICON01 - 1263 )  21260, 11260, 21260
11260 IVPASS = IVPASS + 1
      GO TO 1271
21260 IVFAIL = IVFAIL + 1
 1271 CONTINUE
      IF (IVFAIL /= 0) CALL abort ()
      END
