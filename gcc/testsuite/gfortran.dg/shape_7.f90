! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/52093
!
! Contributed by Mohammad Rahmani
!

Program Main
 Implicit None
 Integer:: X(2,2)
 Integer:: X2(7:11,8:9)

 if (size((X)) /= 4) STOP 1
 if (any (Shape((X))  /= [2,2])) STOP 2
 if (any (lbound((X)) /= [1,1])) STOP 3
 if (any (ubound((X)) /= [2,2])) STOP 4

 if (size(X2) /= 10) STOP 5
 if (any (Shape(X2)  /= [5,2])) STOP 6
 if (any (lbound(X2) /= [7,8]))  STOP 7
 if (any (ubound(X2) /= [11,9])) STOP 8

 if (size((X2)) /= 10) STOP 9
 if (any (Shape((X2))  /= [5,2])) STOP 10
 if (any (lbound((X2)) /= [1,1])) STOP 11
 if (any (ubound((X2)) /= [5,2])) STOP 12
End Program Main

! { dg-final { scan-tree-dump-times "_gfortran_stop" 0 "original" } }

