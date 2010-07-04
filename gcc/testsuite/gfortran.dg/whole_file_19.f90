! { dg-do compile }
! { dg-options "-fwhole-file" }
! Test the fix for pr40011 comment #42, in which the subroutine
! would just get lost with -fwhole-file.
!
! Contributed by Joost VandeVandole <jv244@cam.ac.uk>
!
SUBROUTINE c()
 CALL a()
END SUBROUTINE c

SUBROUTINE a()
END SUBROUTINE a

MODULE M
CONTAINS
 SUBROUTINE b()
   CALL c()
 END SUBROUTINE
END MODULE

USE M
CALL b()
END
! { dg-final { cleanup-modules "m" } }
