C { dg-do compile }
C Option passed to avoid excess errors from obsolete warning
C { dg-options "-w" }

      PROGRAM FM013
      IF (ICZERO) 31270, 1270, 31270
 1270 CONTINUE
 1272 ASSIGN 1273 TO J
 1273 ASSIGN 1274 TO J
 1274 ASSIGN 1275 TO J
      GOTO 1276
 1275 continue
 1276 GOTO J, ( 1272, 1273, 1274, 1275 )
31270 IVDELE = IVDELE + 1
      END

