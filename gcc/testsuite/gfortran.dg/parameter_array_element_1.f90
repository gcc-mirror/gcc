! { dg-do compile }
! { dg-options "-fdump-tree-original" }
! Tests the fix for PR 30872, in which the array element references bo(1,1) etc.
! would be wrong for rank > 1.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
 INTEGER, PARAMETER, DIMENSION(2,3) :: bo= &
                    RESHAPE((/-1,1,-2,2,-3,3/),(/2,3/))
 REAL(kind=8), DIMENSION(  &
          bo(1,1):bo(2,1), &
          bo(1,2):bo(2,2), &
          bo(1,3):bo(2,3)) :: out_val
 out_val=0.0
END
! Scan for the 105 in the declaration real8 out_val[105];
! { dg-final { scan-tree-dump-times "105" 1 "original" } }

