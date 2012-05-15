! { dg-do run }
!
! PR 40450: [F03] procedure pointer as actual argument
!
! Contributed by John McFarland <john.mcfarland@swri.org>

MODULE m
 ABSTRACT INTERFACE
 SUBROUTINE sub()
 END SUBROUTINE sub
 END INTERFACE

CONTAINS

 SUBROUTINE passf(f2)
   PROCEDURE(sub), POINTER:: f2
   CALL callf(f2)
 END SUBROUTINE passf

 SUBROUTINE callf(f3)
   PROCEDURE(sub), POINTER :: f3
   PRINT*, 'calling f'
   CALL f3()
 END SUBROUTINE callf
END MODULE m


PROGRAM prog
 USE m
 PROCEDURE(sub), POINTER :: f1
 f1 => s
 CALL passf(f1)

CONTAINS

 SUBROUTINE s
   PRINT*, 'sub'
 END SUBROUTINE s
END PROGRAM prog
