! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for PR37903, in which the temporary for the vector index
! got the wrong size.
!
! Contributed by Mikael Morin <mikael.morin@tele2.fr>
!
     integer :: i(-1:1) = 1, j(3) = 1, k(3)
      k = j((/1,1,1/)+i)
      end
! { dg-final { scan-tree-dump-times "A\.2\\\[3\\\]" 1 "original" } }
