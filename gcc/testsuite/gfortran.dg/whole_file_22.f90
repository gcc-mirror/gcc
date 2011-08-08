! { dg-do link }
! { dg-options "-fwhole-program -O3 -g" }
!
! PR fortran/40873
!
      program prog
        call one()
        call two()
        call test()
      end program prog
      subroutine one()
        call three()
      end subroutine one
      subroutine two()
        call three()
      end subroutine two
      subroutine three()
      end subroutine three

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

subroutine test()
USE M
CALL b()
END

! { dg-final { cleanup-modules "m" } }
