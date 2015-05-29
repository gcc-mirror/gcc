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

 if (size((X)) /= 4) call abort ()
 if (any (Shape((X))  /= [2,2])) call abort ()
 if (any (lbound((X)) /= [1,1])) call abort ()
 if (any (ubound((X)) /= [2,2])) call abort ()

 if (size(X2) /= 10) call abort ()
 if (any (Shape(X2)  /= [5,2])) call abort ()
 if (any (lbound(X2) /= [7,8]))  call abort ()
 if (any (ubound(X2) /= [11,9])) call abort ()

 if (size((X2)) /= 10) call abort ()
 if (any (Shape((X2))  /= [5,2])) call abort ()
 if (any (lbound((X2)) /= [1,1])) call abort ()
 if (any (ubound((X2)) /= [5,2])) call abort ()
End Program Main

! { dg-final { scan-tree-dump-times "abort" 0 "original" } }

