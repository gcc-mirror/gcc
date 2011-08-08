! { dg-do compile }
! PR 26201: Check that the __convert_*_* functions are treated as intrinsics
! rather than module functions.
! Testcase contributed by Philippe Schaffnit and François-Xavier Coudert.
MODULE MODULE_A
    REAL :: a = 0
END MODULE MODULE_A

MODULE MODULE_B
    REAL :: b = 0
END MODULE MODULE_B

USE MODULE_A
USE MODULE_B
a = 0
END

! { dg-final { cleanup-modules "module_a module_b" } }
