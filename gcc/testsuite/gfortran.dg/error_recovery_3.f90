! { dg-do compile }
! PR30779 incomplete file triggers ICE.
! Note: This file is deliberately cut short to verify a graceful exit. Before
! the patch this gave ICE.
MODULE M1
 INTEGER :: I
END MODULE M1

USE M1,                    ONLY: I,&! { dg-error "Missing" }
! { dg-final { cleanup-modules "m1" } }

